#!/bin/bash
find gl-wrapper -type f -name 'GLW.hs' -exec rm {} \;
find gl-wrapper/GLW -type f -name '*.hs' -exec rm {} \;
./scripts/supplement-spec.sh > glw.xml
docker-compose run --rm app stack run gl-wrapper-gen
cp -r lib-source-files/GLW gl-wrapper/
sudo chown "$(id -gn "$USER")":"$USER" -R gl-wrapper
