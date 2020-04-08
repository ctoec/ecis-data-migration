#!/bin/bash

if [[ $(lsof -i:1402 | grep ssh) ]]; then
	echo "Port 1402 is already in use."
	exit 1
fi

ssh -fN prod.ece-jumpserver.ecereporterpilot.com \
	-L 127.0.0.1:1434:prod.ece-db.ecereporterpilot.com:1433

[[ $? -ne 0 ]] && exit 1

echo "Port forwarding to Hedwig production database established."
echo "Connect to Hedwig production database at 127.0.0.1:1402."
