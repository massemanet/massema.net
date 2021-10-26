#!/bin/bash

DEST=priv/maxmind
mkdir -p "$DEST"
# shellcheck source=~/bin/pet
. "$(command -v pet)" maxmind KEY
if [ -n "$KEY" ]
then for k in Country City ASN
     do URL="https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-$k-CSV&license_key=$KEY&suffix=zip"
        ZIP="/tmp/$$-$k.zip"
        curl -o "$ZIP" "$URL"
        unzip -d "$DEST" "$ZIP"
     done
fi
