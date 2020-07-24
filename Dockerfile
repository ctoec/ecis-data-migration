FROM rocker/r-ubuntu:18.04
RUN apt-get update && apt-get upgrade -y
RUN apt-get install curl gpg gpg-agent -y
# Follow https://docs.microsoft.com/en-us/sql/connect/odbc/linux-mac/installing-the-microsoft-odbc-driver-for-sql-server?view=sql-server-ver15
RUN curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - && \
    curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list > /etc/apt/sources.list.d/mssql-release.list
RUN apt-get update && apt-get install unixodbc-dev libssl-dev -y && ACCEPT_EULA=Y apt-get install msodbcsql17 mssql-tools -y
RUN apt-get install libcurl4-openssl-dev libxml2-dev -y

## Install R packages that aren't already included
RUN R -e "install.packages(c('openxlsx','glue','lubridate','tidyverse','odbc'))"
