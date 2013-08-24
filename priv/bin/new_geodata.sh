#!/bin/bash

DEST=deps/egeoip/priv
if [ -d $DEST ] ; then
    cd $DEST
    wget http://geolite.maxmind.com/download/geoip/database/GeoLiteCity.dat.gz
    gunzip -f GeoLiteCity.dat.gz
    invoke-rc.d massema.net reload-geodata
else
    echo "$DEST doesn't exist"
fi
