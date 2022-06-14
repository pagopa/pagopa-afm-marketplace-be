#!/bin/bash

# This script saves the OpenApi 3 json file.
# Remember to start the application first!!!

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi


curl http://127.0.0.1:8585/v3/api-docs | python3 -m json.tool > ./openapi/openapi.json

