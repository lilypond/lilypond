#!/bin/sh
# The nextline takes the value in parameter one, evaluates it
# (necessary if it contains spaces) and runs it. > redirects
# stdout to the logfile given in parameter 2.  2>&1 redirects
# stderr to stdout (i.e. to the logfile).
eval $1 > $2 2>&1
RetVal=$?  # captures the return value of the command
if [ $RetVal -ne 0 ]; then
	echo
	echo "Please check the logfile" $2 "for errors"
	echo
fi
exit $RetVal
