library(tidyverse)
library(openxlsx)
library(lubridate)
library(glue)
library(DBI)

driver <- "{ODBC Driver 17 for SQL Server}"
server <- "127.0.0.1,1434"
database <- "hedwig"
uid <- "admin"
pwd <- Sys.getenv("HEDWIG_PROD_DB_PASS")
hedwig_con <- dbConnect(odbc::odbc(), 
                 Driver = 'ODBC Driver 17 for SQL Server',
                 Server = server,
                 Database = database,
                 UID = uid, 
                 Pwd = pwd,
                 timeout = 5)
hedwig_query <- function (query) {  res <- dbSendQuery(hedwig_con, query, stringsAsFactors = FALSE)
                                    db_res <- dbFetch(res)
                                    dbClearResult(res)
                                    return(db_res)
                                    }

rates <- read_csv("temporary-psr-generation/rates.csv", col_types = cols(
  Accredited = col_integer(),
  TitleI = col_integer()
))

income_groups <- data.frame(
  NumberOfPeople = seq(1, 12),
  x75SMI = c(43895, 57400, 70906, 84413, 97919, 111452, 113957, 116489, 119022, 121554, 124086, 126619),
  x200FPL = c(24280, 32920, 41560, 50200, 58840, 67480, 76120, 84760, 93400, 102040, 110680, 119320)
)

generate_psr_for_report_id <- function(.id) {
  .report <- hedwig_query(glue("
    select
      Report.Type,
      SubmittedAt,
      OrganizationId,
      Name,
      ReportingPeriodId,
      Period,
      PeriodStart,
      PeriodEnd,
      Accredited,
      C4KRevenue,
      FamilyFeesRevenue
    from Report
    inner join Organization on Organization.Id = OrganizationId
    inner join ReportingPeriod on ReportingPeriod.Id = ReportingPeriodId
    where Report.Id = '{.id}'
  "))
  
  .cdc_fundings <- hedwig_query(glue("
    select
      Child.Id,
      LastName,
      FirstName,
      AgeGroup,
      Time,
      LicenseNumber,
      Region,
      TitleI,
      Entry,
      [Exit],
      NumberOfPeople,
      Income,
      Foster,
      Sasid
    from Funding
    inner join Enrollment on Enrollment.Id = EnrollmentId
    inner join Site on Site.Id = SiteId
    inner join Child on Child.Id = ChildId
    left join (
      select
        FamilyId,
        Income,
        NumberOfPeople
      from (
        select
          FamilyId,
          Income,
          NumberOfPeople,
    
          row_number() over (
            partition by FamilyId
            order by DeterminationDate desc
          ) as rn
      
        from FamilyDetermination
      ) as T
      where rn = 1
    ) as MostRecentFamilyDetermination
      on MostRecentFamilyDetermination.FamilyId = Child.FamilyId
    where Site.OrganizationId = {.report$OrganizationId}
      and Source = {.report$Type}
      and FirstReportingPeriodId <= {.report$ReportingPeriodId}
      and (LastReportingPeriodId is null or LastReportingPeriodId >= {.report$ReportingPeriodId})
  ")) %>%
    mutate(
      AgeGroup = case_when(
        AgeGroup == 0 ~ "Infant/Toddler",
        AgeGroup == 1 ~ "Preschool",
        AgeGroup == 2 ~ "School-age"
      ),
      Time = case_when(
        Time == 0 ~ "FT",
        Time == 1 ~ "PT"
      ),
      Region = case_when(
        Region == 0 ~ "East",
        Region == 1 ~ "NorthCentral",
        Region == 2 ~ "NorthWest",
        Region == 3 ~ "SouthCentral",
        Region == 4 ~ "SouthWest",
      ),
      Accredited = .report$Accredited,
      NumberOfPeople = if_else(Foster == 0, NumberOfPeople, as.integer(1)),
      Income = if_else(Foster == 0, Income, 0)
    ) %>%
    left_join(rates, by = c("Accredited", "TitleI", "Region", "AgeGroup", "Time"))
  
  .c4k_fundings <- hedwig_query(glue("
    select
      Child.Id,
      Funding.FamilyId
    from Funding
    inner join Enrollment on Enrollment.Id = EnrollmentId
    inner join Site on Site.Id = SiteId
    inner join Child on Child.Id = ChildId
    where Site.OrganizationId = {.report$OrganizationId}
      and Source = 1 -- C4K
      and CertificateStartDate <= '{.report$PeriodEnd}'
      and (CertificateEndDate is null or CertificateEndDate >= '{.report$PeriodStart}')
  "))
  
  .funding_spaces <- hedwig_query(glue("
    select
      AgeGroup,
      Time,
      Capacity
    from FundingSpace
    where OrganizationId = {.report$OrganizationId}
      and Source = {.report$Type}
  ")) %>%
    mutate(
      AgeGroup = case_when(
        AgeGroup == 0 ~ "Infant/Toddler",
        AgeGroup == 1 ~ "Preschool",
        AgeGroup == 2 ~ "School-age"
      ),
      Time = case_when(
        Time == 0 ~ "FT",
        Time == 1 ~ "PT"
      )
    )
  
  .enrollment_roster <- .cdc_fundings %>%
    left_join(.c4k_fundings, by = "Id") %>%
    left_join(income_groups %>% transmute(NumberOfPeople, MaxIncome = x75SMI), by = "NumberOfPeople") %>%
    arrange(AgeGroup, LastName, FirstName) %>%
    transmute(
      `#` = row_number(),
      `SASID #` = Sasid,
      `Last Name` = LastName,
      `First Name` = FirstName,
      `C4K Family ID` = FamilyId,
      `C4K (Y/N)` = if_else(is.na(FamilyId), "N", "Y"),
      `Center (License Number)` = LicenseNumber,
      `Service (Space Type)` = glue("{Time} {AgeGroup}"),
      `Weekly Reimbursement Rate` = Rate,
      `Enrollment Date` = format(Entry, "%m/%d/%Y"),
      `Termination Date` = format(Exit, "%m/%d/%Y"),
      `# in Family` = NumberOfPeople,
      `Annual Income` = Income,
      `Eligible for State Funds (Y Or N*)` = if_else(Income <= MaxIncome, "Y", "N*")
    )
  
  .family_income_grouping <- .cdc_fundings %>%
    left_join(income_groups, by = "NumberOfPeople") %>%
    filter(Income <= x200FPL) %>%
    count(NumberOfPeople) %>%
    right_join(income_groups, by = "NumberOfPeople") %>%
    replace_na(list(n = 0)) %>%
    transmute(
      `Family Size` = NumberOfPeople,
      `Less than or Equal to` = x200FPL,
      `# of Families` = n
    )
  
  .spaces <- .cdc_fundings %>%
    count(AgeGroup, Time, Accredited, TitleI, Rate) %>%
    inner_join(.funding_spaces, by = c("AgeGroup", "Time")) %>%
    transmute(
      AgeGroup,
      Time,
      Accredited = if_else(Accredited == 1, "Acc.", "Una"),
      TitleI = if_else(TitleI == 1, "Title I", "Standard"),
      Rate,
      Capacity,
      Utilization = n
    )
  
  .psr <- .report %>%
    transmute(
      Contractor = Name,
      `Reporting Period` = glue("{format(PeriodStart, '%B %d, %Y')} - {format(PeriodEnd, '%B %d, %Y')}"),
      `Number of Weeks` = as.numeric((as_date(PeriodEnd) - as_date(PeriodStart) + 1) / 7),
      `Family Fees` = FamilyFeesRevenue,
      `# Subsidies` = nrow(.c4k_fundings),
      Subsidies = C4KRevenue,
      Date = format(SubmittedAt, "%m/%d/%Y")
    )
  
  .path <- glue("temporary-psr-generation/Output/{.report$Name} {format(.report$Period, '%B %Y')} CDC PSR.xlsx")
  
  .wb <- loadWorkbook("temporary-psr-generation/psr-template.xlsx")
  writeData(.wb, sheet = 2, .enrollment_roster, headerStyle = createStyle(textDecoration = "BOLD"))
  writeData(.wb, sheet = 3, .family_income_grouping, headerStyle = createStyle(textDecoration = "BOLD"))
  writeData(.wb, sheet = 4, .spaces, headerStyle = createStyle(textDecoration = "BOLD"))
  writeData(.wb, sheet = 5, .psr, headerStyle = createStyle(textDecoration = "BOLD"))
  saveWorkbook(.wb, .path)
}


