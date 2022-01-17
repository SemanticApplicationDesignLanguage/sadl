#!/bin/sh
####################################################################
## This shell script changes a user and their files to a new UID/GID
####################################################################

# Ensure errors stop execution
set -eu

# Get arguments
USER=$1
GROUP=$2
OLD_UID=$3
OLD_GID=$4
NEW_UID=$5
NEW_GID=$6

# This script works only if run by root
if [ "$(id -u)" != "0" ]; then
    echo "Must run $0 as root"
    exit 1
fi

# Change the user's UID
if [ "$OLD_UID" != "$NEW_UID" ]; then
    echo "Changing UID of $USER from $OLD_UID to $NEW_UID"
    usermod -u "$NEW_UID" -o "$USER"
    find / -xdev -user "$OLD_UID" -exec chown -h "$USER" {} \;
fi

# Change the user's GID
if [ "$OLD_GID" != "$NEW_GID" ]; then
    echo "Changing GID of $GROUP from $OLD_GID to $NEW_GID"
    groupmod -g "$NEW_GID" -o "$GROUP"
    find / -xdev -group "$OLD_GID" -exec chgrp -h "$GROUP" {} \;
fi
