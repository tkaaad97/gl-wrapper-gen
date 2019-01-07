#!/bin/bash
OUTPUT_PATH='src'
COPY_PATH=${COPY_PATH:='../gl-wrapper/'}
mkdir -p "$OUTPUT_PATH"/GLW
find "$OUTPUT_PATH" -type f -name 'GLW.hs' -exec rm {} \;
find "$OUTPUT_PATH"/GLW -type f -name '*.hs' -exec rm {} \;
./scripts/supplement-spec.sh > glw.xml
docker-compose run --rm app stack run gl-wrapper-gen -- "$OUTPUT_PATH"
sudo chown "$(id -gn "$USER")":"$USER" -R "$OUTPUT_PATH"
cp -r "$OUTPUT_PATH" "$COPY_PATH"
