#!/bin/bash
find gl-wrapper -type f -name 'GLW.hs' -exec rm {} \;
find gl-wrapper/GLW -type f -name '*.hs' -exec rm {} \;
docker-compose run --rm app stack clean
