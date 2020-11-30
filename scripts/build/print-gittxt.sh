#!/bin/sh

set -eu

src="$1"

if test -d ${src}/.git
then
    cd ${src}
    BR=`git rev-parse --symbolic-full-name HEAD | sed s#^refs/heads/##`
    HD=`git rev-parse --verify HEAD`
    if git rev-parse -q origin/master ; then
        FP=`git merge-base --octopus origin/master HEAD`
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
        git log --pretty=format:"      HASH: %H%n   SUBJECT: %s%n" $FP~1..HEAD
    else
        echo "MERGE_BASE: unknown"
        echo
        echo "   HISTORY:"
        echo "   ========"
        git log --max-count=10 --pretty=format:"      HASH: %H%nSUBJECT: %s%n"
    fi
    echo ""
    date
else
    echo ".git missing"
fi
