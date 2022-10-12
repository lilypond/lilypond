#!/bin/sh

set -eu

src="$1"

if test -e ${src}/.git
then
    cd ${src}
    BR=`git rev-parse --abbrev-ref HEAD`
    HD=`git rev-parse --verify HEAD`
    if [ -n "${CI_MERGE_REQUEST_DIFF_BASE_SHA+set}" ]; then
        # Use the data provided by CI, origin/master may not exist.
        FP="$CI_MERGE_REQUEST_DIFF_BASE_SHA"
    elif git rev-parse origin/stable/2.24 >/dev/null; then
        FP=`git merge-base --octopus origin/stable/2.24 HEAD`
    else
        FP=""
    fi
    echo "    BRANCH: $BR"
    echo "      HEAD: $HD"
    if [ ! -z $FP ]
    then
        echo "MERGE_BASE: $FP"
        echo
        echo "   HISTORY:"
        echo "   ========"
        git --no-pager log --pretty=format:"      HASH: %H%n   SUBJECT: %s%n" HEAD $FP^!
    else
        echo "MERGE_BASE: unknown"
        echo
        echo "   HISTORY:"
        echo "   ========"
        git --no-pager log --max-count=10 --pretty=format:"      HASH: %H%nSUBJECT: %s%n"
    fi
    echo ""
    date
else
    echo ".git missing"
fi
