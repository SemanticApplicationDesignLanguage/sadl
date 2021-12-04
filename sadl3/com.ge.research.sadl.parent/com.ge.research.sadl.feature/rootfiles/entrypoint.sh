#!/bin/sh
#####################################################################################
## This shell script is our Docker image's entrypoint for running SADLCli as any user
#####################################################################################

# Ensure errors stop execution
set -eu

# Go to this script's directory
cd -P -- "$(dirname -- "$0")"

# If RUN_AS is defined, change sadl user to that UID/GID and run
# SADLCli using that UID/GID instead
if [ -n "${RUN_AS:-}" ]; then
    # shellcheck disable=SC2086
    ./change-user.sh sadl sadl 1000 1000 $RUN_AS
    exec setpriv --reuid=sadl --regid=sadl --clear-groups ./SADLCli.sh "$@"
else
    exec ./SADLCli.sh "$@"
fi
