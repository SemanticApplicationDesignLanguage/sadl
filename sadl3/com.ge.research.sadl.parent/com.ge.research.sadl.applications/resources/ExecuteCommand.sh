#!/bin/sh
##############################################################################
## This shell script is the main entry point into SADL command-line interface.
##############################################################################

cd -P -- "$(dirname -- "$0")"

LOG4J_CONFIGURATION_FILE=log4j2.properties
export LOG4J_CONFIGURATION_FILE

./eclipse -application com.ge.research.sadl.applications.ExecuteCommand -consolelog -data ../workspace -noSplash "$@"
