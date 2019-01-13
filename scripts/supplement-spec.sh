#!/bin/bash
injects1=$(xmlstarlet sel -B -t -c '.' objects.xml)
injects2=$(xmlstarlet sel -B -t -c '.' newtypes.xml)
spec=$(cat gl.xml)

while read -r inject
do
    command='xmlstarlet ed -P '
    while read -r action
    do
        method=$(xmlstarlet sel -t -v '/action/@method' <<<"$action")
        xpath=$(xmlstarlet sel -t -v '/action/@xpath' <<<"$action")
        type=$(xmlstarlet sel -t -v '/action/@type' <<<"$action")
        name=$(xmlstarlet sel -t -v '/action/@name' <<<"$action")
        value=$(xmlstarlet sel -t -v '/action/@value' <<<"$action")
        command+="--${method} '""${xpath}""' -t \"${type}\" -n \"${name}\" -v \"${value}\" "
    done < <(xmlstarlet sel -t -m '/inject/action' -c '.' --nl <<<"$inject")
    spec=$(echo "$spec" | bash -c "$command")
done < <({
    xmlstarlet sel -t -m '/objects/object/injects/inject' -c '.' --nl <<<"$injects1";
    xmlstarlet sel -t -m '/newtypes/newtype/injects/inject' -c '.' --nl <<<"$injects2";
})

echo "$spec"
