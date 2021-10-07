#!/bin/sh
##############################################################################
## This shell script is the main entry point into SADL command-line interface.
##############################################################################

# Ensure errors stop execution
set -eu

# Ensure DISPLAY is defined
XRUN=
if [ -z "${DISPLAY:-}" ]; then
    XRUN="xvfb-run --auto-servernum --error-file=xrun.err"
fi

# Ensure logging goes to console
LOG4J_CONFIGURATION_FILE=log4j2.properties
export LOG4J_CONFIGURATION_FILE

# Go to this script's directory
cd -P -- "$(dirname -- "$0")"

# Run SADL with our command line arguments
exec ${XRUN} ./eclipse -application com.ge.research.sadl.applications.ExecuteCommand -consolelog -data workspace -noSplash "$@"
