#!/bin/bash

# This script enables `emacsclient` within the Windows Subsystem for
# Linux (WSL) to communicate with Emacs for Windows. WSL sees a
# different filesystem layout than Emacs does, so invoking emacsclient
# directly will lead to confusion.

# For pathnames in the /mnt/$DRIVE/... hierarchy, the script
# translates them to the $DRIVE:/... format native Windows apps will
# understand.

# For pathnames "inside" the WSL environment, the script copies them
# to a temporary directory, waits for Emacs to be done, then copies
# them back.

# KNOWN ISSUES
#
# This works for `git commit`, but if the repo is inside the WSL
# environment, magit won't be able to find it, and will immediately
# quit emacsclient with an error.
#
# Pathnames with spaces probably break this thing.

# We assume that Emacs can read and write files wherever
# EMACS_SERVER_FILE is located.

if [[ ! -v EMACS_SERVER_FILE ]]; then
    echo "fatal: EMACS_SERVER_FILE is not set!"
    exit 1
fi

TMPDIR=$(dirname $EMACS_SERVER_FILE)

# ARGS is an array of the arguments to emacsclient. TMP_FILES is a map
# where the key is WSL path to the file, and the value is the path to
# a temporary copy that is visible outside of WSL.

declare -a ARGS=()
declare -A TMP_FILES=()

option_pattern="^[-+]"
windows_path_pattern="^/mnt/[a-z]/"

while [[ $# > 0 ]]; do
    if [[ $1 =~ $option_pattern ]]; then
        ARGS+=($1)
    else
        rp=$(realpath $1)
        if [[ $rp =~ $windows_path_pattern ]]; then
	    wp="${rp:5:1}:${rp:6}"
        else
            tp="$(mktemp -p $TMPDIR --directory)/$(basename $rp)"
            cp -v $rp $tp
            TMP_FILES[$rp]=$tp
            wp="${tp:5:1}:${tp:6}"
        fi
        ARGS+=($wp)
    fi
    shift
done

emacsclient ${ARGS[@]}

for rp in ${!TMP_FILES[@]}; do
    tp=${TMP_FILES[$rp]}
    cp -v $tp $rp
    rm -v -r $(dirname $tp)
done
