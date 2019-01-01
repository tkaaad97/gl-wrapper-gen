#!/bin/bash
find gl-wrapper -type f -name 'GLW.hs' -exec rm {} \;
find gl-wrapper/GLW -type f -name '*.hs' ! -name 'Classes.hs' -exec rm {} \;
./scripts/supplement-spec.sh > glw.xml
docker-compose run --rm app stack run gl-wrapper-gen
sudo chown "$(id -gn "$USER")":"$USER" -R gl-wrapper
