#!/bin/bash

DEST=priv/maxmind
mkdir -p "$DEST"
. /home/masse/bin/pet maxmind MMKEY
if [ -n "$MMKEY" ]
then for k in Country City ASN
     do URL="https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-$k-CSV&license_key=$MMKEY&suffix=zip"
        ZIP="/tmp/$$-$k.zip"
        curl -o "$ZIP" "$URL"
        DATE="$(unzip -l /tmp/554032-ASN.zip | grep LICENSE.txt | grep -Eo '[2-9][0-9][0-9][0-9][0-1][0-9][0-3][0-9]')"
        mkdir -p "$DEST/$DATE"
        unzip -d "$DEST/$DATE" "$ZIP"
     done
fi
