#!/bin/bash
#
# Serve guts with nc, by parsing the command from the first line of stdin
#
# Copy stderr to stdout with tee, but also keep it on stderr
#   - stdout (with copy of stderr) is sent back to the client
#   - stderr is printed on the console (i.e., docker log)
#
err="$(mktemp)"
read -r command
echo "# Executing ./guts $command" >&2
bash -c "exec ./guts $command" 2> >(tee "$err" >&2)
cat "$err" && rm "$err"
