#!/bin/bash
#
# Main entry point for docker container
#
set -eou pipefail

# check for daemons to be started
./daemons check

# start listening on port 31337 for incoming connections
nc -p 31337 -ll -e ./guts-serve.sh
