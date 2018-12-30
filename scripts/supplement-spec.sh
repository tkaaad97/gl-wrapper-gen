#!/bin/bash
injects=$(xmlstarlet sel -B -t -c '.' objects.xml)
spec=$(cat gl.xml)

while read -r inject
do
    xpath=$(xmlstarlet sel -t -v '/inject/@xpath' <<<"$inject")
    attribute=$(xmlstarlet sel -t -v '/inject/@attribute' <<<"$inject")
    value=$(xmlstarlet sel -t -v '/inject/@value' <<<"$inject")
    spec=$(echo "$spec" | xmlstarlet ed -P -i "$xpath" -t attr -n "$attribute" -v "$value")
done < <(xmlstarlet sel -t -m '/objects/object/injects/inject' -c . --nl <<<"$injects")

echo "$spec"
