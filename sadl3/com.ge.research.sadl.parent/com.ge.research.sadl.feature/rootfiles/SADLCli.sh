#!/bin/sh
###################################################################
## This shell script runs SADL's command-line interface application
###################################################################

# Ensure errors stop execution
set -eu

# Go to this script's directory
cd -P -- "$(dirname -- "$0")"

# Ensure DISPLAY is defined
XRUN=
if [ -z "${DISPLAY:-}" ]; then
    XRUN="xvfb-run --auto-servernum --error-file=xrun.err"
fi

# Ensure logging goes to console
LOG4J_CONFIGURATION_FILE=log4j2.properties
export LOG4J_CONFIGURATION_FILE

# Run SADLCli application with given command line arguments
# shellcheck disable=SC2086
exec ${XRUN} ./eclipse -application com.ge.research.sadl.applications.SADLCli -consolelog -data workspace -noSplash "$@"
