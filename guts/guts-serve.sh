#!/bin/bash
#
# Serve guts with nc, by parsing the command from the first line of stdin
#
# Copy stderr to stdout with tee, but also keep it on stderr
#   - stdout (with copy of stderr) is sent back to the client
#   - stderr is printed on the console (i.e., docker log)
#
set -euo pipefail

read -a command
if [[ ${command[0]} == "list-submissions" ]]; then
    # list homework submission files
    semester=${command[1]}
    class=${command[2]}
    canonical_name=${command[3]}
    cd "${GUTS_WORK_DIR}/hwks/${semester}/${class}/mails" &&
        ls -1rt "${canonical_name}"*
elif [[ ${command[0]} == "cat-submission" ]]; then
    semester=${command[1]}
    class=${command[2]}
    mailfile=${command[3]}
    cat "${GUTS_WORK_DIR}/hwks/${semester}/${class}/mails/${mailfile}"
elif [[ ${command[0]} == "list-reports" ]]; then
    # list homework test reports
    semester=${command[1]}
    class=${command[2]}
    suite=${command[3]}
    canonical_name=${command[4]}
    cd "${GUTS_WORK_DIR}/hwks/${semester}/${class}/reports/${suite}" &&
        ls "${canonical_name}"
elif [[ ${command[0]} == "cat-report" ]]; then
    # download homework test report
    semester=${command[1]}
    class=${command[2]}
    suite=${command[3]}
    canonical_name=${command[4]}
    cat "${GUTS_WORK_DIR}/hwks/${semester}/${class}/reports/${suite}/${canonical_name}"
else
    # execute guts command
    echo "# Executing ./guts ${command[*]}" >&2
    err="$(mktemp)"
    bash -c "exec ./guts ${command[*]}" 2> >(tee "$err" >&2)
    cat "$err" && rm "$err"
fi
