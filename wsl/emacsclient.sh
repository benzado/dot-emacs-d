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

# This works for `git commit`, but if the repo is inside the WSL
# environment, magit won't be able to find it, and will immediately
# quit emacsclient with an error.

# Pathnames with spaces probably break this thing.

# My Windows username is hardcoded in the TMPDIR, because it needs to
# be a directory under /mnt/c/.

TMPDIR=/mnt/c/Users/Benjamin/tmp

declare -A TMP_FILES=()
declare -a ARGS=()

mkdir -p $TMPDIR

while [[ $# > 0 ]]; do
    case $1 in
	-* | +*)
	    ARGS+=($1)
	    ;;
	/mnt/?/*)
	    rp=$(realpath $1)
	    wp="${rp:5:1}:${rp:6}"
	    ARGS+=($wp)
	    ;;
	*)
            bn=$(basename $1)
            td=$(mktemp -p $TMPDIR --directory)
	    rp="${td}/${bn}"
	    cp -v $1 $rp
	    TMP_FILES[$1]=$rp
	    wp="${rp:5:1}:${rp:6}"
	    ARGS+=($wp)
	    ;;
    esac
    shift
done

emacsclient ${ARGS[@]}

for wp in ${!TMP_FILES[@]}; do
    tp=${TMP_FILES[$wp]}
    cp -v $tp $wp
    rm -v -r $(dirname $tp)
done
