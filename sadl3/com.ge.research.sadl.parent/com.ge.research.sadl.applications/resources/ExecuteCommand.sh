#!/bin/sh
##############################################################################
## This shell script is the main entry point into SADL command-line interface.
##############################################################################
WORKSPACE=../workspace
cd -P -- "$(dirname -- "$0")"
./eclipse -console -noSplash -data $WORKSPACE -application com.ge.research.sadl.applications.ExecuteCommand "$@"
