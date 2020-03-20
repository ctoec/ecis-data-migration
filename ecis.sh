#!/bin/bash

# Restores a backup of ECIS to a SQL Server Docker container
# (We look for the backup at ECIS/ECIS_Backup.bak)

# -----------------------
#  ENVIRONMENT VARIABLES
# -----------------------

# Define environment variables in .Renviron (see .Renviron.example)
#
#   * DB_NAME: The name of the database being restored from the backup
#
#   * DB_PASS: A password that meets SQL Server's default password strength requirements
#
#   * ECIS_BACKUP_ENCRYPTION_CERT_NAME: The name for the TDE certificate used to encrypt the backup
#         (We look for the certificate and private key at ECIS/[CERT_NAME]_BEC.cer and ECIS/[CERT_NAME]_Prv.key)
#
#   * ECIS_BACKUP_ENCRYPTION_CERT_SECRET: The password for the TDE certifcate used to encrypt the backup

# -----------------------
#        FUNCTIONS
# -----------------------

docker_cp () {
	docker cp $1 $2 &> /dev/null
	if [[ $? -ne 0 ]]; then
		echo "Could not copy file ${1} to container."
		exit 1
	fi
}

run_sql () {
	if [[ $# -eq 0  ]]; then
		read -d '' query "$@" # read heredoc into variable
	else
		query=$1
	fi

	docker exec ecis /opt/mssql-tools/bin/sqlcmd -S localhost -U SA -P $DB_PASS -Q "$query"
	[[ $? -ne 0 ]] && exit 1
}

# ----------------------
#         SCRIPT
# ----------------------

# Check if docker is running
docker version > /dev/null
[[ $? -ne 0 ]] && exit 1

# Check if ECIS container already exists
if [[ $(docker ps -aqf name=ecis) ]]; then
	echo "'ecis' container already exists."
	exit 1
fi

# Load environment variables
export $(cat .Renviron | xargs)

if \
	[[ -z "${DB_NAME}" ]] || \
	[[ -z "${DB_PASS}" ]] || \
	[[ -z "${ECIS_BACKUP_ENCRYPTION_CERT_NAME}" ]] || \
	[[ -z "${ECIS_BACKUP_ENCRYPTION_CERT_SECRET}" ]]
then
	echo "Missing environment variables."
	exit 1
fi

# Create SQL Server Docker container
echo -n "Creating 'ecis' container: "

docker run -e "ACCEPT_EULA=Y" -e "MSSQL_SA_PASSWORD=${DB_PASS}1" \
	--name "ecis" -p 1401:1433 \
	-v ecisdata:/var/opt/mssql \
	-d mcr.microsoft.com/mssql/server:latest

# Replace password to make SQL Server happy
docker exec ecis /opt/mssql-tools/bin/sqlcmd \
	-S localhost -U SA -P "${DB_PASS}1" \
	-Q "ALTER LOGIN SA WITH PASSWORD='${DB_PASS}'"

# Copy certs and backup file to container
echo "Copying files to container..."

docker exec ecis mkdir /var/opt/mssql/backup /var/opt/mssql/certs
docker_cp "ECIS/${ECIS_BACKUP_ENCRYPTION_CERT_NAME}_BEC.cer" ecis:/var/opt/mssql/certs
docker_cp "ECIS/${ECIS_BACKUP_ENCRYPTION_CERT_NAME}_Prv.key" ecis:/var/opt/mssql/certs
docker_cp ECIS/ECIS_Backup.bak ecis:/var/opt/mssql/backup

echo "Restoring ${DB_NAME}..."

# Create master key
run_sql "CREATE MASTER KEY ENCRYPTION BY PASSWORD = '${DB_PASS}'"

# Create certificate for decrypting backup
run_sql <<- SQL
	CREATE CERTIFICATE ${ECIS_BACKUP_ENCRYPTION_CERT_NAME}
		FROM FILE = '/var/opt/mssql/certs/${ECIS_BACKUP_ENCRYPTION_CERT_NAME}_BEC.cer'
		WITH PRIVATE KEY (
			FILE = '/var/opt/mssql/certs/${ECIS_BACKUP_ENCRYPTION_CERT_NAME}_Prv.key',
			DECRYPTION BY PASSWORD = '${ECIS_BACKUP_ENCRYPTION_CERT_SECRET}'
		)
SQL

# Restore database
run_sql <<- SQL
	RESTORE DATABASE ${DB_NAME}
		FROM DISK = '/var/opt/mssql/backup/ECIS_Backup.bak' WITH
			MOVE '${DB_NAME}2' TO '/var/opt/mssql/data/${DB_NAME}.mdf',
			MOVE '${DB_NAME}2_log' TO '/var/opt/mssql/data/${DB_NAME}_log.ldf'
SQL

echo "${DB_NAME} database successfully restored."
echo "Connect to ${DB_NAME} database at 127.0.0.1:1401."
