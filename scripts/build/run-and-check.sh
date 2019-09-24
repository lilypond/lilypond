#!/bin/sh
#
# The next code line takes the value in parameter one, evaluates
# it (necessary if it contains spaces), and runs it.
#
# `>' redirects stdout to the logfile given in parameter 2.
# `2>&1' redirects stderr to stdout (i.e., to the logfile).

eval $1 > $2 2>&1
RetVal=$? # capture return value of the just executed command
if [ $RetVal -ne 0 ]; then
	CurrDir=`pwd` # get current working directory
	echo
	echo "Please check the logfile"
	echo
	echo "  $CurrDir/$2"
	echo
	echo "for errors"
	echo
fi
exit $RetVal
