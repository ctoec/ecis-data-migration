library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(readxl)
library(glue)
library(fuzzyjoin)

# Towns by CDC payment regions

region_e_towns <- c("Ashford","Bozrah","Brooklyn","Canterbury","Chaplin","Colchester","Columbia","Coventry","East Lyme","Eastford","Franklin","Griswold","Groton","Hampton","Killingly","Lebanon","Ledyard","Lisbon","Mansfield","Montville","New London","North Stonington","Norwich","Plainfield","Pomfret","Preston","Putnam","Salem","Scotland","Sprague","Sterling","Stonington","Thompson","Union","Voluntown","Waterford","Willington","Windham","Woodstock")
region_nc_towns <- c("Andover","Avon","Berlin","Bloomfield","Bolton","Bristol","Burlington","Canton","East Granby","East Hartford","East Windsor","Ellington","Enfield","Farmington","Glastonbury","Granby","Hartford","Hebron","Manchester","Marlborough","New Britain","Newington","Plainville","Plymouth","Rocky Hill","Simsbury","Somers","Southington","South Windsor","Stafford","Suffield","Tolland","Vernon","West Hartford","Wethersfield","Windsor","Windsor Locks")
region_nw_towns <- c("Barkhamsted","Beacon Falls","Bethel","Bethlehem","Bridgewater","Brookfield","Canaan","Cheshire","Colebrook","Cornwall","Danbury","Goshen","Hartland","Harwinton","Kent","Litchfield","Middlebury","Morris","Naugatuck","New Fairfield","New Hartford","New Milford","Newtown","Norfolk","North Canaan","Oxford","Prospect","Redding","Ridgefield","Roxbury","Salisbury","Sandy Hook", "Sharon","Sherman","Southbury","Thomaston","Torrington","Warren","Washington","Waterbury","Watertown","Winchester","Wolcott","Woodbury")
region_sc_towns <- c("Ansonia","Bethany","Branford","Chester","Clinton","Cromwell","Deep River","Derby","Durham","East","Haddam","East Hampton","East Haven","Essex","Guilford","Haddam","Hamden","Killingworth","Lyme","Madison","Meriden","Middletown","Middlefield","Milford","New Haven","North Branford","North Haven","Old Lyme","Old Saybrook","Orange","Portland","Seymour","Shelton","Wallingford","West Haven","Westbrook","Woodbridge")
region_sw_towns <- c("Bridgeport","Darien","Easton","Fairfield","Greenwich","Monroe","New Canaan","Norwalk","Stamford","Stratford","Trumbull","Weston","Westport","Wilton")

towns <- data.frame(
  town = c(region_e_towns, region_nc_towns, region_nw_towns, region_sc_towns, region_sw_towns),
  region = c(
    rep("e", times = length(region_e_towns)),
    rep("nc", times = length(region_nc_towns)),
    rep("nw", times = length(region_nw_towns)),
    rep("sc", times = length(region_sc_towns)),
    rep("sw", times = length(region_sw_towns))
  ),
  stringsAsFactors = FALSE
)

# Program list

programs <- read_xls(
  "All State-Funded Programs Contact List 2019-20.xls",
  skip = 1,
  col_types = c(
    rep("skip", times = 6),
    "text",
    rep("skip", times = 25),
    rep("text", times = 6),
    "skip",
    rep("text", times = 2),
    rep("skip", times = 42)
  ),
  col_names = c(
    "contractor",
    "name",
    "address",
    "town",
    "zip",
    "license_no",
    "naeyc_id",
    "registry_id",
    "facility_code"
  )
) %>%
  filter(!is.na(name)) %>%
  mutate(
    license_no = ifelse(license_no %in% c("Exempt", "?"), NA, license_no),
    org_name = trimws(lapply(strsplit(contractor, ","), tail, n = 1))
  )

# Reporting periods

reporting_periods <- data.frame(period = seq(ymd('2019-07-01'), ymd('2029-07-01'), by = 'months')) %>%
  mutate(
    id = row_number(),
    funding_source = "CDC",
    period_start = period + (2 - ((wday(period)) %% 7)), # closest monday to start of month
    period_end = lead(period_start) - 1,
    due_at = lead(period) + (20 - (wday(lead(period)) %% 7)), # third friday of following month
    psr_format = gsub("\\s+", " ", paste(format(period_start, "%B %e, %Y"), "-", format(period_end - 2, "%B %d, %Y")))
  ) %>%
  head(-1)

# CDC PSR data

psr_enrollment <- data.frame()
psr_revenue <- data.frame()

for (.file in list.files("PSRs")) {
  if (.file == ".keep" | startsWith(.file, "~")) { next; }

  .path <- file.path(".", "PSRs", .file)

  .period <- read_xlsx(.path, range = "G4", col_names = "psr_format", col_types = "text") %>%
    mutate(psr_format = gsub(" to ", " - ", psr_format)) %>%
    left_join(reporting_periods, by = "psr_format")
  
  .roster <- read_xlsx(
    .path,
    sheet = "Enrollee Roster",
    skip = 5,
    col_types = c("skip", rep("text", times = 7), "numeric", rep("date", times = 2), rep("numeric", times = 2), "skip"),
    col_names = c(
      "sasid",
      "last_name",
      "first_name",
      "c4k_case_no",
      "c4k",
      "license_no",
      "age_group",
      "reimbursement_rate",
      "enrollment_date",
      "termination_date",
      "no_in_family",
      "annual_income"
    )
  ) %>%
    filter(!is.na(license_no)) %>%
    mutate(
      source = .file,
      period = .period$id
    ) %>%
    stringdist_inner_join(
      filter(programs, !is.na(license_no) & !is.na(org_name)),
      by = "license_no",
      max_dist = 1,
      method = "lv",
      distance_col = "fuzzy"
    ) %>%
    group_by(sasid) %>%
    arrange(fuzzy) %>%
    slice(1) %>%
    ungroup()
  
  psr_enrollment <- rbind(psr_enrollment, .roster)
  
  .organization <- .roster %>%
    select(org_name) %>%
    unique()

  if (nrow(.organization) > 1) {
    stop(glue("Found {nrow(.organization)} organizations in file {.file}"))
  }
  
  .revenue <- read_xlsx(
    .path,
    range = "H32:L37",
    col_types = c("numeric", "skip", "numeric", "skip", "numeric"),
    col_names = c("family_fees", "c4k_count", "c4k_revenue")
  ) %>%
    mutate(organization = .organization$org_name, period = .period$id) %>%
    group_by(organization, period) %>%
    summarize(family_fees = sum(family_fees, na.rm = TRUE), c4k_revenue = sum(c4k_revenue, na.rm = TRUE)) %>%
    ungroup()
  
  psr_revenue <- rbind(psr_revenue, .revenue)
}

WARNINGS.psr_enrollment <- psr_enrollment %>%
  filter(fuzzy > 0) %>%
  glue_data("Fuzzy matched a child from file '{source}' to {org_name}; license number as written: {license_no.x}, matched to: {license_no.y}")

# Organizations

organizations <- psr_revenue %>%
  select(name = organization) %>%
  unique() %>%
  mutate(organization_id = row_number())

# Sites

sites <- programs %>%
  inner_join(organizations, by = c("org_name" = "name")) %>%
  left_join(towns, by = "town") %>%
  select(name, organization_id, license_no, naeyc_id, registry_id, facility_code, region) %>%
  mutate(site_id = row_number(), title_i = FALSE)

## TODO: Find Title I status for sites

# ECIS data
# Run ecis.sh to load data into a Docker container
# Data is flattened into one row per Student/EnrollmentFunding combination

con <- dbConnect(odbc(),
                 Driver   = "ODBC Driver 17 for SQL Server",
                 Server   = "127.0.0.1,1401",
                 Database = Sys.getenv("DB_NAME"),
                 UID      = "SA",
                 PWD      = Sys.getenv("DB_PASS"))

query <- function (query) { return(dbGetQuery(con, query)) }

ecis_flat <- query(glue("
  select
    Student.Enrollment.FacilityCode,
    Student.Student.SASID,
    Student.StudentDetails.FirstName,
    Student.StudentDetails.MiddleName,
    Student.StudentDetails.LastName,
    Student.StudentDetails.Dob,
    Student.StudentDetails.Gender,
    case
      when HispanicOrLatinxEthnicity.Value is null
      then 0 else 1
    end as HispanicOrLatinxEthnicity,
    case
      when AmericanIndianOrAlaskaNative.Value is null
      then 0 else 1
    end as AmericanIndianOrAlaskaNative,
    case
      when Asian.Value is null
      then 0 else 1
    end as Asian,
    case
      when BlackOrAfricanAmerican.Value is null
      then 0 else 1
    end as BlackOrAfricanAmerican,
    case
      when NativeHawaiianOrPacificIslander.Value is null
      then 0 else 1
    end as NativeHawaiianOrPacificIslander,
    case
      when White.Value is null
      then 0 else 1
    end as White,
    case
      when OtherRace.Value is null
      then 0 else 1
    end as OtherRace,
    case
      when Student.Address.AddressType = 'Foster Parent'
      then 1 else 0
    end as Foster,
    Student.Address.StreetNumber,
    Student.Address.Address1,
    Student.Address.Address2,
    Student.Address.Town,
    Student.Address.State,
    Student.Address.Zip,
    Enrollment.AdditionalStudentInfo.NumberOfPeopleInHousehold,
    Enrollment.AdditionalStudentInfo.AnnualFamilyIncome,
    Enrollment.AdditionalStudentInfo.DateFamilyIncomeDocumented,
    case
      when Enrollment.AdditionalStudentInfo.FamilyIncomeNotDisclosed = 'TRUE'
      then 1 else 0
    end as FamilyIncomeNotDisclosed,
    Student.Enrollment.EnrollmentDate,
    Student.Enrollment.FacilityExitDate,
    Student.Enrollment.ExitCategory,
    Student.Enrollment.ExitReason,
    Enrollment.EnrollmentFunding.FundingType,
    Enrollment.EnrollmentFunding.SpaceType,
    Enrollment.EnrollmentFunding.StartDate as FundingStartDate,
    Enrollment.EnrollmentFunding.EndDate as FundingEndDate,
    AdditionalFundingSource.AdditionalFundingSource,
    Enrollment.EnrollmentFunding.DateCreated,
    Enrollment.EnrollmentFunding.ModifiedDate,
    BirthCertificateId.Value as BirthCertificateId,
    StateOfBirth.Value as StateOfBirth,
    TownOfBirth.Value as TownOfBirth
  from (
    select
      Enrollment.EnrollmentFunding.Id,
      row_number() over (
        partition by
          Enrollment.EnrollmentFunding.StartDate,
          Enrollment.EnrollmentFunding.EndDate,
          Enrollment.EnrollmentFunding.FundingType,
          Enrollment.EnrollmentFunding.SpaceType,
          Student.Enrollment.StudentId
    
        order by Enrollment.EnrollmentFunding.ModifiedDate desc
      ) as rn
  
    from Enrollment.EnrollmentFunding
  
    left join Student.Enrollment on Enrollment.EnrollmentFunding.EnrollmentId = Student.Enrollment.Id
  
    where 1=1
  ) as UndupEnrollmentFunding
    
  left join Enrollment.EnrollmentFunding on UndupEnrollmentFunding.Id = Enrollment.EnrollmentFunding.Id
  left join Student.Enrollment on Enrollment.EnrollmentFunding.EnrollmentId = Student.Enrollment.Id
  left join Student.Student on Student.Enrollment.StudentId = Student.Student.Id
  left join Student.StudentDetails on Student.Student.Id = Student.StudentDetails.StudentId /* 3 duplicate records */
  left join (
    select StudentId, Id,
      row_number() over (
        partition by StudentId
        order by ModifiedBy desc
      ) as rn
    from Student.Address
  ) as UndupStudentAddress on Student.Student.Id = UndupStudentAddress.StudentId
  left join Student.Address on UndupStudentAddress.Id = Student.Address.Id
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Hispanic'
  ) as HispanicOrLatinxEthnicity on Student.Student.Id = HispanicOrLatinxEthnicity.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'American Indian or Alaska Native'
  ) as AmericanIndianOrAlaskaNative on Student.Student.Id = AmericanIndianOrAlaskaNative.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Asian'
  ) as Asian on Student.Student.Id = Asian.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Black or African American'
  ) as BlackOrAfricanAmerican on Student.Student.Id = BlackOrAfricanAmerican.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Native Hawaiian or Other Pacific Islander'
  ) as NativeHawaiianOrPacificIslander on Student.Student.Id = NativeHawaiianOrPacificIslander.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'White'
  ) as White on Student.Student.Id = White.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Other'
  ) as OtherRace on Student.Student.Id = OtherRace.StudentId
  left join (
    select StudentId, ID,
      row_number() over (
        partition by StudentId
        order by DateFamilyIncomeDocumented desc
      ) as rn
    from Enrollment.AdditionalStudentInfo
  ) as UndupAdditionalStudentInfo on Student.Student.Id = UndupAdditionalStudentInfo.StudentId
  left join Enrollment.AdditionalStudentInfo on UndupAdditionalStudentInfo.ID = Enrollment.AdditionalStudentInfo.ID
  left join (
    select EnrollmentId,
      case
        when count(*) > 1
        then 'Multiple Additional Funding Sources'
        else MIN(AdditionalFundingType)
      end as AdditionalFundingSource
    from (select distinct EnrollmentId, AdditionalFundingType from Enrollment.AdditionalFundingSources) as T
    group by EnrollmentId
  ) as AdditionalFundingSource on Student.Enrollment.Id = AdditionalFundingSource.EnrollmentId
  left join Universal.Agency on Student.Enrollment.FacilityCode = Universal.Agency.Code
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 3
  ) as BirthCertificateId on Student.Student.Id = BirthCertificateId.StudentId
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 4
  ) as StateOfBirth on Student.Student.Id = StateOfBirth.StudentId
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 5
  ) as TownOfBirth on Student.Student.Id = TownOfBirth.StudentId
  
  where UndupEnrollmentFunding.rn = 1
    and (UndupStudentAddress.rn is null or UndupStudentAddress.rn = 1)
    and (UndupAdditionalStudentInfo.rn is null or UndupAdditionalStudentInfo.rn = 1)
    
    and Student.Enrollment.FacilityCode in (
      '{paste(sites$facility_code, collapse = \"','\")}'
    )
"))

# Child and family info

ecis_enrolled_students <- ecis_flat %>%
  # filter(
  #   (is.na(FacilityExitDate) & is.na(FundingEndDate)) |
  #     FacilityExitDate >= reporting_periods$period_start[1] |
  #     FundingEndDate >= reporting_periods$period_start[1]
  # ) %>%
  # select(SASID) %>%
  # unique() %>%
  # left_join(ecis_flat, by = "SASID") %>%
  group_by(SASID) %>%
  arrange(ModifiedDate) %>%
  slice(n()) %>%
  ungroup() %>%
  select(
    SASID,
    FacilityCode,
    FirstName,
    MiddleName,
    LastName,
    Dob,
    Gender,
    HispanicOrLatinxEthnicity,
    AmericanIndianOrAlaskaNative,
    Asian,
    BlackOrAfricanAmerican,
    NativeHawaiianOrPacificIslander,
    White,
    OtherRace,
    Foster,
    StreetNumber,
    Address1,
    Address2,
    Town,
    State,
    Zip,
    NumberOfPeopleInHousehold,
    AnnualFamilyIncome,
    DateFamilyIncomeDocumented,
    FamilyIncomeNotDisclosed,
    BirthCertificateId,
    StateOfBirth,
    TownOfBirth
  )

psr_students_with_sasids <- psr_enrollment %>%
  filter(!is.na(sasid)) %>%
  select(sasid) %>%
  unique() %>%
  left_join(psr_enrollment, by = "sasid") %>%
  group_by(sasid) %>%
  arrange(period) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(organizations, by = c("org_name" = "name")) %>%
  select(
    sasid,
    last_name,
    first_name,
    organization_id,
    c4k_case_no,
    c4k,
    age_group,
    reimbursement_rate,
    enrollment_date,
    termination_date,
    no_in_family,
    annual_income
  )

psr_students_without_sasids <- psr_enrollment %>%
  filter(is.na(sasid)) %>%
  left_join(organizations, by = c("org_name" = "name")) %>%
  select(
    sasid,
    first_name,
    last_name,
    organization_id,
    c4k_case_no,
    c4k,
    age_group,
    reimbursement_rate,
    enrollment_date,
    termination_date,
    no_in_family,
    annual_income
  ) %>%
  unique()

psr_students_with_later_sasids <- psr_students_without_sasids %>%
  rbind(psr_students_with_sasids) %>%
  group_by(first_name, last_name, organization_id, enrollment_date) %>%
  filter(sum(is.na(sasid)) > 0 & length(unique(sasid)) == 2)

if (nrow(psr_students_with_later_sasids) > 0) {
  stop(glue("Found {nrow(psr_students_with_later_sasids)} students who were given a SASID after appearing on a PSR without one"))
}
# TODO: There are none, so I'm not going to worry about merging records for now

psr_students <- psr_students_with_sasids %>%
  rbind(psr_students_without_sasids) %>%
  filter(is.na(termination_date) | termination_date >= reporting_periods$period_start[1])

psr_students_not_enrolled_in_ecis <- psr_students %>%
  mutate(exists_in_ecis = sasid %in% ecis_flat$SASID) %>%
  filter(!exists_in_ecis) %>%
  select(sasid, first_name, last_name)

ecis_not_enrolled_students <- query(glue("
  select
    Student.Student.RegisteringSiteId,
    Student.Student.SASID,
    Student.StudentDetails.FirstName,
    Student.StudentDetails.MiddleName,
    Student.StudentDetails.LastName,
    Student.StudentDetails.Dob,
    Student.StudentDetails.Gender,
    case
      when HispanicOrLatinxEthnicity.Value is null
      then 0 else 1
    end as HispanicOrLatinxEthnicity,
    case
      when AmericanIndianOrAlaskaNative.Value is null
      then 0 else 1
    end as AmericanIndianOrAlaskaNative,
    case
      when Asian.Value is null
      then 0 else 1
    end as Asian,
    case
      when BlackOrAfricanAmerican.Value is null
      then 0 else 1
    end as BlackOrAfricanAmerican,
    case
      when NativeHawaiianOrPacificIslander.Value is null
      then 0 else 1
    end as NativeHawaiianOrPacificIslander,
    case
      when White.Value is null
      then 0 else 1
    end as White,
    case
      when OtherRace.Value is null
      then 0 else 1
    end as OtherRace,
    case
      when Student.Address.AddressType = 'Foster Parent'
      then 1 else 0
    end as Foster,
    Student.Address.StreetNumber,
    Student.Address.Address1,
    Student.Address.Address2,
    Student.Address.Town,
    Student.Address.State,
    Student.Address.Zip,
    Enrollment.AdditionalStudentInfo.NumberOfPeopleInHousehold,
    Enrollment.AdditionalStudentInfo.AnnualFamilyIncome,
    Enrollment.AdditionalStudentInfo.DateFamilyIncomeDocumented,
    case
      when Enrollment.AdditionalStudentInfo.FamilyIncomeNotDisclosed = 'TRUE'
      then 1 else 0
    end as FamilyIncomeNotDisclosed,
    BirthCertificateId.Value as BirthCertificateId,
    StateOfBirth.Value as StateOfBirth,
    TownOfBirth.Value as TownOfBirth
  from Student.Student
  left join Student.StudentDetails on Student.Student.Id = Student.StudentDetails.StudentId /* 3 duplicate records */
  left join (
    select StudentId, Id,
      row_number() over (
        partition by StudentId
        order by ModifiedBy desc
      ) as rn
    from Student.Address
  ) as UndupStudentAddress on Student.Student.Id = UndupStudentAddress.StudentId
  left join Student.Address on UndupStudentAddress.Id = Student.Address.Id
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Hispanic'
  ) as HispanicOrLatinxEthnicity on Student.Student.Id = HispanicOrLatinxEthnicity.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'American Indian or Alaska Native'
  ) as AmericanIndianOrAlaskaNative on Student.Student.Id = AmericanIndianOrAlaskaNative.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Asian'
  ) as Asian on Student.Student.Id = Asian.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Black or African American'
  ) as BlackOrAfricanAmerican on Student.Student.Id = BlackOrAfricanAmerican.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Native Hawaiian or Other Pacific Islander'
  ) as NativeHawaiianOrPacificIslander on Student.Student.Id = NativeHawaiianOrPacificIslander.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'White'
  ) as White on Student.Student.Id = White.StudentId
  left join (
    select distinct StudentId, 1 as Value
    from Student.Race where RaceCode = 'Other'
  ) as OtherRace on Student.Student.Id = OtherRace.StudentId
  left join (
    select StudentId, ID,
      row_number() over (
        partition by StudentId
        order by DateFamilyIncomeDocumented desc
      ) as rn
    from Enrollment.AdditionalStudentInfo
  ) as UndupAdditionalStudentInfo on Student.Student.Id = UndupAdditionalStudentInfo.StudentId
  left join Enrollment.AdditionalStudentInfo on UndupAdditionalStudentInfo.ID = Enrollment.AdditionalStudentInfo.ID
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 3
  ) as BirthCertificateId on Student.Student.Id = BirthCertificateId.StudentId
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 4
  ) as StateOfBirth on Student.Student.Id = StateOfBirth.StudentId
  left join (
    select StudentId, Value
    from Student.EditableFieldValues
    where Student.EditableFieldValues.EditableFieldId = 5
  ) as TownOfBirth on Student.Student.Id = TownOfBirth.StudentId
  
  where (UndupStudentAddress.rn is null or UndupStudentAddress.rn = 1)
    and (UndupAdditionalStudentInfo.rn is null or UndupAdditionalStudentInfo.rn = 1)
    
    and Student.Student.SASID in (
      '{paste(psr_students_not_enrolled_in_ecis$sasid, collapse = \"','\")}'
    )
")) %>%
  select(
    SASID,
    FacilityCode = RegisteringSiteId,
    FirstName,
    MiddleName,
    LastName,
    Dob,
    Gender,
    HispanicOrLatinxEthnicity,
    AmericanIndianOrAlaskaNative,
    Asian,
    BlackOrAfricanAmerican,
    NativeHawaiianOrPacificIslander,
    White,
    OtherRace,
    Foster,
    StreetNumber,
    Address1,
    Address2,
    Town,
    State,
    Zip,
    NumberOfPeopleInHousehold,
    AnnualFamilyIncome,
    DateFamilyIncomeDocumented,
    FamilyIncomeNotDisclosed,
    BirthCertificateId,
    StateOfBirth,
    TownOfBirth
  )
  
ecis_students <- ecis_enrolled_students %>%
  rbind(ecis_not_enrolled_students)

students <- psr_students %>%
  left_join(ecis_students, by=c("sasid"="SASID")) %>%
  mutate(student_id = row_number())

additional_sasid_data <- read_csv("sasid-fill.csv", col_types = cols(
  SASID = col_character(),
  Birthdate = col_date(format = "%m/%d/%Y")
)) %>%
  unique()

# Children

children <- students %>%
  transmute(
    child_id = student_id,
    Sasid = sasid,
    FirstName = first_name,
    MiddleName,
    LastName = last_name,
    Suffix = NA,
    Birthdate = ymd(Dob),
    BirthTown = TownOfBirth,
    BirthState = StateOfBirth,
    BirthCertificateId,
    AmericanIndianOrAlaskaNative = as.logical(AmericanIndianOrAlaskaNative),
    Asian = as.logical(Asian),
    BlackOrAfricanAmerican = as.logical(BlackOrAfricanAmerican),
    NativeHawaiianOrPacificIslander = as.logical(NativeHawaiianOrPacificIslander),
    White = as.logical(White),
    HispanicOrLatinxEthnicity = as.logical(HispanicOrLatinxEthnicity),
    Gender = case_when(
      is.na(Gender) ~ 4,
      Gender == "M " ~ 0,
      Gender == "F " ~ 1
    ),
    Foster = as.logical(Foster),
    family_id = student_id,
    organization_id
  ) %>%
  mutate(
    HispanicOrLatinxEthnicity = if_else(
      AmericanIndianOrAlaskaNative | Asian | BlackOrAfricanAmerican | NativeHawaiianOrPacificIslander | White,
      HispanicOrLatinxEthnicity,
      NA
    )
  ) %>%
  replace_na(list(
    AmericanIndianOrAlaskaNative = FALSE,
    Asian = FALSE,
    BlackOrAfricanAmerican = FALSE,
    NativeHawaiianOrPacificIslander = FALSE,
    White = FALSE,
    Foster = FALSE
  )) %>%
  left_join(additional_sasid_data, by = c("Sasid" = "SASID")) %>%
  mutate(Birthdate = if_else(!is.na(Birthdate.x), Birthdate.x, Birthdate.y)) %>%
  select(-Birthdate.x, -Birthdate.y)

families <- students %>%
  transmute(
    family_id = student_id,
    AddressLine1 = ifelse(
      is.na(StreetNumber) | is.na(Address1),
      NA,
      glue("{StreetNumber} {Address1}")
    ),
    AddressLine2 = Address2,
    Town,
    State,
    Zip,
    Homelessness = FALSE
  )

family_determinations <- students %>%
  transmute(
    family_determination_id = student_id,
    family_id = student_id,
    NotDisclosed = FALSE,
    NumberOfPeople = no_in_family,
    Income = annual_income,
    DeterminationDate = if_else(
      no_in_family == NumberOfPeopleInHousehold &
        abs(annual_income - AnnualFamilyIncome) < 1,
      as_date(DateFamilyIncomeDocumented),
      as_date(NA)
    )
  )

# Enrollment and funding

unique_students_missing_sasids <- psr_enrollment %>%
  filter(is.na(sasid)) %>%
  select(sasid, first_name, last_name) %>%
  unique()

if (nrow(unique_students_missing_sasids) > 1) {
  stop(glue("There are {nrow(unique_students_missing_sasids)} students missing SASIDs; need to implement a way to uniquely join to children"))
}

ecis_enrollments <- ecis_flat %>%
  mutate(
    age_group = case_when(
      grepl("Infant/Toddler", SpaceType) ~ "IT",
      grepl("Preschool", SpaceType) ~ "PS",
      grepl("School Age", SpaceType) ~ "SA"
    )
  )

fix_non_uniqued_enrollments <- ecis_enrollments %>%
  filter(SASID %in% c("2387887382", "5650287478", "8877296962")) %>%
  group_by(SASID) %>%
  mutate(
    EnrollmentDate = min(EnrollmentDate),
    FundingStartDate = min(FundingStartDate)
  ) %>%
  arrange(FacilityExitDate) %>%
  slice(n()) %>%
  ungroup()

ecis_enrollments <- ecis_enrollments %>%
  filter(!(SASID %in% c("2387887382", "5650287478", "8877296962"))) %>%
  rbind(fix_non_uniqued_enrollments)

c4k_recipients <- psr_enrollment %>%
  filter(c4k == "Y") %>%
  group_by(sasid) %>%
  mutate(
    first_observed_c4k_period = min(period),
    last_observed_c4k_period = max(period)
  ) %>%
  arrange(period) %>%
  slice(n()) %>%
  ungroup() %>%
  select(sasid, first_observed_c4k_period, last_observed_c4k_period)

psr_enrollment_uniqued <- psr_enrollment %>%
  filter(is.na(termination_date) | termination_date >= reporting_periods$period_start[1]) %>%
  mutate(age_group = if_else(age_group == "Preschool F/T", "PS", age_group)) %>%
  group_by(sasid, age_group, facility_code) %>%
  mutate(
    first_observed_psr_period = min(period),
    last_observed_psr_period = max(period)
  ) %>%
  arrange(period) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(c4k_recipients, by = "sasid") %>%
  left_join(select(children, Sasid, child_id), by = c("sasid" = "Sasid")) %>%
  left_join(select(sites, facility_code, site_id), by = "facility_code") %>%
  left_join(ecis_enrollments, by = c("sasid" = "SASID", "age_group", "facility_code" = "FacilityCode")) %>%
  group_by(sasid, age_group, facility_code) %>%
  # Get rid of what appear to be accidental enrollments if there is an active enrollment
  # Does not support reenrollments, or students who are no longer enrolled and will error for these cases
  filter(n() == 1 | sum(is.na(FacilityExitDate)) != 1 | is.na(FacilityExitDate)) %>%
  ungroup() %>%
  mutate(enrollment_id = row_number())
  
nonunique_enrollments <- psr_enrollment_uniqued %>%
  group_by(sasid, age_group) %>%
  filter(n() > 1)

if (nrow(nonunique_enrollments) > 0) {
  stop(glue("Could not unique {nrow(nonunique_enrollments)} students"))
}

enrollments <- psr_enrollment_uniqued %>%
  transmute(
    enrollment_id,
    child_id,
    site_id,
    AgeGroup = case_when(
      age_group == "IT" ~ 0,
      age_group == "PS" ~ 1,
      age_group == "SA" ~ 2
    ),
    Entry = if_else(
      !is.na(enrollment_date),
      as_date(enrollment_date),
      if_else(
        !is.na(EnrollmentDate),
        as_date(EnrollmentDate),
        as_date(NA)
      )
    ),
    Exit = if_else(
      !is.na(termination_date),
      as_date(termination_date),
      if_else(
        !is.na(FacilityExitDate),
        as_date(FacilityExitDate),
        as_date(NA)
      )
    ),
    ExitReason = case_when(
      ExitCategory == "Moved to Another Town" ~ "Moved within Connecticut",
      ExitCategory == "Other" ~ "Other",
      ExitCategory == "Parent Withdrew Child" ~ "Other",
      ExitCategory == "Aged Out" ~ "Aged out",
      ExitCategory == "Child Stopped Attending" ~ "Stopped attending",
      ExitCategory == "Chose to Attend A Different Program" ~ "Chose to attend a different program",
      !is.na(Exit) ~ "Unknown"
    )
  )

# TODO: Add additional ExitCategories

cdc_fundings <- psr_enrollment_uniqued %>%
  transmute(
    enrollment_id,
    FundingSource = 0,
    FamilyId = as.numeric(NA),
    CertificateStartDate = as_date(NA),
    CertificateEndDate = as_date(NA),
    FirstReportingPeriodId = first_observed_psr_period,
    LastReportingPeriodId = if_else (
      last_observed_psr_period < max(psr_enrollment$period),
      last_observed_psr_period,
      as.integer(NA)
    ),
    FundingTime = if_else(age_group == "SA", 1, 0)
  )

c4k_fundings <- psr_enrollment_uniqued %>%
  inner_join(select(reporting_periods, id, period_start), by = c("first_observed_c4k_period" = "id")) %>%
  inner_join(select(reporting_periods, id, period_end), by = c("last_observed_c4k_period" = "id")) %>%
  transmute(
    enrollment_id,
    FundingSource = 1,
    FamilyId = as.integer(gsub("[^0-9]", "", c4k_case_no)),
    CertificateStartDate = period_start,
    CertificateEndDate = if_else(
      last_observed_c4k_period < max(psr_enrollment$period),
      period_end,
      as_date(NA)
    ),
    FirstReportingPeriodId = as.integer(NA),
    LastReportingPeriodId = as.integer(NA),
    FundingTime = as.integer(NA)
  )

fundings <- cdc_fundings %>%
  rbind(c4k_fundings) %>%
  mutate(funding_id = row_number())

# Reports

past_reports <- psr_revenue %>%
  left_join(organizations, by = c("organization" = "name")) %>%
  transmute(
    FundingSource = 0,
    ReportingPeriodId = period,
    SubmittedAt = Sys.time(),
    organization_id,
    Accredited = TRUE,
    C4KRevenue = c4k_revenue,
    RetroactiveC4KRevenue = FALSE,
    FamilyFeesRevenue = family_fees
  )

current_reports <- organizations %>%
  transmute(
    FundingSource = 0,
    ReportingPeriodId = max(psr_enrollment$period) + 1,
    SubmittedAt = as.POSIXct(NA),
    organization_id,
    Accredited = TRUE,
    C4KRevenue = as.numeric(NA),
    RetroactiveC4KRevenue = FALSE,
    FamilyFeesRevenue = as.numeric(NA)
  )

reports <- past_reports %>%
  rbind(current_reports) %>%
  mutate(report_id = row_number())

# Temporary fix for lack of multi-site support
sites <- sites %>% filter(site_id != 2)
enrollments <- enrollments %>%
  mutate(site_id = if_else(site_id == 2, as.integer(1), site_id))

# Warnings

WARNINGS.psr_enrollment %>%
  lapply(warning, call. = FALSE)

# Clean up

rm(c4k_fundings)
rm(c4k_recipients)
rm(cdc_fundings)
rm(current_reports)
rm(ecis_enrolled_students)
rm(ecis_enrollments)
rm(ecis_flat)
rm(ecis_not_enrolled_students)
rm(ecis_students)
rm(fix_non_uniqued_enrollments)
rm(nonunique_enrollments)
rm(past_reports)
rm(psr_enrollment)
rm(psr_enrollment_uniqued)
rm(psr_revenue)
rm(psr_students)
rm(psr_students_not_enrolled_in_ecis)
rm(psr_students_with_later_sasids)
rm(psr_students_with_sasids)
rm(psr_students_without_sasids)
rm(region_e_towns)
rm(region_nc_towns)
rm(region_nw_towns)
rm(region_sc_towns)
rm(region_sw_towns)
rm(students)
rm(towns)
rm(unique_students_missing_sasids)

# LOAD

hedwig_con <- dbConnect(odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "127.0.0.1,1402",
                        Database = "hedwig",
                        UID      = "admin",
                        PWD      = Sys.getenv("HEDWIG_PROD_DB_PASS"))

hedwig_query <- function (query) { return(dbGetQuery(hedwig_con, query)) }

# hedwig_query("delete from Permission")
# hedwig_query("delete from [User]")
# hedwig_query("delete from Funding")
# hedwig_query("delete from Enrollment")
# hedwig_query("delete from Site")
# hedwig_query("delete from Child")
# hedwig_query("delete from FamilyDetermination")
# hedwig_query("delete from Family")
# hedwig_query("delete from Report")
# hedwig_query("delete from Organization")
# hedwig_query("delete from ReportingPeriod")

# reporting_period_query_values <- reporting_periods %>%
#   mutate(
#     funding_source = case_when(
#       funding_source == 'CDC' ~ 0
#     ),
#     period = format(period, format = "%Y-%m-%d"),
#     period_start = format(period_start, format = "%Y-%m-%d"),
#     period_end = format(period_end, format = "%Y-%m-%d"),
#     due_at = format(due_at, format = "%Y-%m-%d"),
#   ) %>%
#   apply(1, function(x) glue(gsub("\n", "",
# "(
# {x['funding_source']},
# '{x['period']}',
# '{x['period_start']}',
# '{x['period_end']}',
# '{x['due_at']}'
# )"
#   ))) %>% paste0(collapse = ", ")
# 
# reporting_periods <- hedwig_query(glue("
#   insert into ReportingPeriod (Type, Period, PeriodStart, PeriodEnd, DueAt)
#   output Inserted.Id
#   values {reporting_period_query_values}
# ")) %>%
#   select(id.REMOTE = Id) %>%
#   cbind(reporting_periods)

reporting_periods <- hedwig_query("select Period, Id from ReportingPeriod") %>%
  mutate(id.REMOTE = Id, period = as_date(Period)) %>%
  right_join(reporting_periods, by = "period")

load_organization_to_prod <- function(.organization_id) {
  .organization <- organizations %>% filter(organization_id == .organization_id)
  
  .organization <- hedwig_query(glue("
    insert into Organization (Name)
    output Inserted.Id
    values ('{.organization$name}')
  ")) %>%
    select(organization_id.REMOTE = Id) %>%
    cbind(.organization)
  
  .sites <- sites %>%
    inner_join(select(.organization, organization_id, organization_id.REMOTE), by = "organization_id")
  
  .sites <- .sites %>%
    mutate(
      region = case_when(
        region == 'e' ~ 0,
        region == 'nc' ~ 1,
        region == 'nw' ~ 2,
        region == 'sc' ~ 3,
        region == 'sw' ~ 4
      )
    ) %>%
    select(name, title_i, region, organization_id.REMOTE) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Site (Name, TitleI, Region, OrganizationId)
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(site_id.REMOTE = Id) %>%
    cbind(.sites)
  
  .families <- families %>%
    inner_join(select(children, family_id, organization_id), by = "family_id") %>%
    inner_join(select(.organization, organization_id, organization_id.REMOTE), by = "organization_id")
  
  .families <- .families %>%
    select(AddressLine1, AddressLine2, Town, State, Zip, Homelessness, organization_id.REMOTE) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Family (AddressLine1, AddressLine2, Town, State, Zip, Homelessness, OrganizationId)
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(family_id.REMOTE = Id) %>%
    cbind(.families)
  
  .determinations <- family_determinations %>%
    inner_join(select(.families, family_id, family_id.REMOTE), by = "family_id")
  
  .determinations <- .determinations %>%
    mutate(
      DeterminationDate = format(DeterminationDate, format = "%Y-%m-%d")
    ) %>%
    select(NotDisclosed, NumberOfPeople, Income, DeterminationDate, family_id.REMOTE) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into FamilyDetermination (NotDisclosed, NumberOfPeople, Income, DeterminationDate, FamilyId)
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(determination_id.REMOTE = Id) %>%
    cbind(.determinations)
  
  .children <- children %>%
    inner_join(select(.organization, organization_id, organization_id.REMOTE), by = "organization_id") %>%
    inner_join(select(.families, family_id, family_id.REMOTE), by = "family_id")
  
  .children <- .children %>%
    mutate(
      Birthdate = format(Birthdate, format = "%Y-%m-%d")
    ) %>%
    select(
      Sasid, FirstName, MiddleName, LastName, Suffix,
      Birthdate, BirthTown, BirthState, BirthCertificateId,
      AmericanIndianOrAlaskaNative, Asian, BlackOrAfricanAmerican,
      NativeHawaiianOrPacificIslander, White, HispanicOrLatinxEthnicity,
      Gender, Foster, family_id.REMOTE, organization_id.REMOTE
    ) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    mutate(Guid = "NEWID()") %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Child (
        Sasid, FirstName, MiddleName, LastName, Suffix,
        Birthdate, BirthTown, BirthState, BirthCertificateId,
        AmericanIndianOrAlaskaNative, Asian, BlackOrAfricanAmerican,
        NativeHawaiianOrPacificIslander, White, HispanicOrLatinxEthnicity,
        Gender, Foster, FamilyId, OrganizationId, Id
      )
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(child_id.REMOTE = Id) %>%
    cbind(.children)
  
  .enrollments <- enrollments %>%
    inner_join(select(.sites, site_id, site_id.REMOTE), by = "site_id") %>%
    inner_join(select(.children, child_id, child_id.REMOTE), by = "child_id")
  
  .enrollments <- .enrollments %>%
    mutate(
      Entry = format(Entry, format = "%Y-%m-%d"),
      Exit = format(Exit, format = "%Y-%m-%d")
    ) %>%
    select(AgeGroup, Entry, Exit, ExitReason, child_id.REMOTE, site_id.REMOTE) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Enrollment (AgeGroup, Entry, [Exit], ExitReason, ChildId, SiteId)
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(enrollment_id.REMOTE = Id) %>%
    cbind(.enrollments)
  
  .fundings <- fundings %>%
    inner_join(select(.enrollments, enrollment_id, enrollment_id.REMOTE), by = "enrollment_id") %>%
    left_join(select(reporting_periods, FirstReportingPeriodId = id, FirstReportingPeriodId.REMOTE = id.REMOTE), by = "FirstReportingPeriodId") %>%
    left_join(select(reporting_periods, LastReportingPeriodId = id, LastReportingPeriodId.REMOTE = id.REMOTE), by = "LastReportingPeriodId")
  
  .fundings <- .fundings %>%
    mutate(
      CertificateStartDate = format(CertificateStartDate, format = "%Y-%m-%d"),
      CertificateEndDate = format(CertificateEndDate, format = "%Y-%m-%d")
    ) %>%
    select(
      FundingSource, FamilyId, CertificateStartDate, CertificateEndDate,
      FirstReportingPeriodId.REMOTE, LastReportingPeriodId.REMOTE,
      FundingTime, enrollment_id.REMOTE
    ) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Funding (
        Source, FamilyId, CertificateStartDate, CertificateEndDate,
        FirstReportingPeriodId, LastReportingPeriodId, Time, EnrollmentId
      )
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(funding_id.REMOTE = Id) %>%
    cbind(.fundings)
  
  .reports <- reports %>%
    inner_join(select(.organization, organization_id, organization_id.REMOTE), by = "organization_id") %>%
    inner_join(select(reporting_periods, ReportingPeriodId = id, ReportingPeriodId.REMOTE = id.REMOTE), by = "ReportingPeriodId")
  
  .reports <- .reports %>%
    mutate(
      SubmittedAt = format(SubmittedAt, format = "%Y-%m-%dT%H:%M:%S")
    ) %>%
    select(
      FundingSource, ReportingPeriodId.REMOTE, SubmittedAt, organization_id.REMOTE,
      Accredited, C4KRevenue, RetroactiveC4KRevenue, FamilyFeesRevenue
    ) %>%
    mutate_all(function(x) dbQuoteLiteral(hedwig_con, x)) %>%
    apply(1, function(x) glue("({paste0(x, collapse = ',')})")) %>%
    paste0(collapse = ",") %>%
    (function(x) glue("
      insert into Report (
        Type, ReportingPeriodId, SubmittedAt, OrganizationId,
        Accredited, C4KRevenue, RetroactiveC4KRevenue, FamilyFeesRevenue
      )
      output Inserted.Id
      values {x}
    ")) %>%
    hedwig_query() %>%
    select(report_id.REMOTE = Id) %>%
    cbind(.reports)
}

load_user_with_org_permission_to_prod <- function(.winged_keys_id, .first_name, .last_name, .organization_id) {
  .user_id <- glue("
      insert into [User] (WingedKeysId, FirstName, LastName)
      output Inserted.Id
      values ('{.winged_keys_id}', '{.first_name}', '{.last_name}')
    ") %>%
    hedwig_query()
  
  .organization <- organizations %>%
    filter(organization_id == .organization_id)
  
  .organization_id <- glue("
    select Id from Organization where Name = '{.organization$name}'
  ") %>%
    hedwig_query()
    
  glue("
    insert into Permission (Type, UserId, OrganizationId)
    values ('Organization', {.user_id$Id}, {.organization_id$Id})
  ") %>%
    hedwig_query()
}

load_cdc_funding_space_to_prod <- function(.organization_id, .time, .age, .capacity) {
  .organization <- organizations %>%
    filter(organization_id == .organization_id)
  
  .organization_id <- glue("
    select Id from Organization where Name = '{.organization$name}'
  ") %>%
    hedwig_query()

  glue("
    insert into FundingSpace (OrganizationId, Capacity, Source, Time, AgeGroup)
    values ({.organization_id$Id}, {.capacity}, 0, {.time}, {.age})
  ") %>%
    hedwig_query()
}
