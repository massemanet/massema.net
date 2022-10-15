#!/usr/bin/env bash

# to release (from masse)
rebar3 release
rebar3 tar
sudo su - luser tar -C ~/massema.net -xzf /home/masse/git/massema.net/_build/default/rel/massema.net/massema.net-*.tar.gz

# to update geodata
bin/update_geodata.sh
bin/massema.net rpc egeoip reload
