#!/bin/bash

if [[ "$(pwd)" =~ .*"openapi".* ]]; then
    cd ..
fi

mvn test -Dtest=OpenApiGenerationTest

jq 'del( .paths[].post, .paths[].put, .paths[].delete)' ./openapi/openapi.json > ./openapi/openapi-technical-support.json

removeList=$(jq '.paths | to_entries[] | select(.value.get == null) | .key' ./openapi/openapi-technical-support.json)

for element in $removeList
do
  cmd="jq 'del(.paths[${element}])' ./openapi/openapi-technical-support.json > ./openapi/temp.json.temp && mv ./openapi/temp.json.temp ./openapi/openapi-technical-support.json"
  eval $cmd
done

jq '.info.title = "Marketplace API for Technical Support" | .info.description = "marketplace-technical-support"' ./openapi/openapi-technical-support.json > ./openapi/temp.json.temp && mv ./openapi/temp.json.temp ./openapi/openapi-technical-support.json