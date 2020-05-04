#!/bin/sh

cd "$(dirname "$0")" \
&& cd ../browser-app \
&& cp -rf ../yarn.lock . \
&& NEXT_VERSION=`npm view sadl-extension@next version` \
&& mv -f ./package.json ./package.json.original \
&& cat ./package.json.original | jq .version=\"$NEXT_VERSION\" | jq .dependencies.\"sadl-extension\"=\"$NEXT_VERSION\" > package.json \
&& docker build --build-arg "GITHUB_TOKEN=$GITHUB_TOKEN" --no-cache . -t theiaide/sadl:$NEXT_VERSION \
&& docker tag theiaide/sadl:$NEXT_VERSION theiaide/sadl:next \
&& rm ./package.json \
&& mv -f ./package.json.original ./package.json
