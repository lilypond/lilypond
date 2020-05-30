#!/bin/sh

# Usage run-and-check COMMAND LOGFILE

# Get current working directory; this must be done first since
# parameter 1 could contain a directory change.
CurrDir=`pwd`
mkdir -p $(dirname $2)
# The next code line takes the value in parameter 1, evaluates
# it (necessary if it contains spaces), and runs it.
#
# `>' redirects stdout to the logfile given in parameter 2.
# `2>&1' redirects stderr to stdout (i.e., to the logfile).
eval $1 > $2 2>&1

# Capture return value of the just executed command.
RetVal=$?
if [ $RetVal -ne 0 ]; then
	echo
	echo "Please check the logfile"
	echo
	echo "  $CurrDir/$2"
	echo
	echo "for errors"
	echo
	echo "last 20 lines:"
	tail -20 "$CurrDir/$2"
fi
exit $RetVal
