#!/bin/sh
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2012--2022  Phil Holmes <mail@philholmes.net>
#
# LilyPond is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# LilyPond is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.


# Usage run-and-check COMMAND LOGFILE

# Get current working directory; this must be done first since
# parameter 1 could contain a directory change.
CurrDir=`pwd`
mkdir -p $(dirname $2)

echo "Command: $1" > $2
echo "" >> $2

# The next code line takes the value in parameter 1, evaluates
# it (necessary if it contains spaces), and runs it.
#
# `>' redirects stdout to the logfile given in parameter 2.
# `2>&1' redirects stderr to stdout (i.e., to the logfile).
eval $1 >> $2 2>&1

# Capture return value of the just executed command.
RetVal=$?
if [ $RetVal -ne 0 ]; then
	cp "$CurrDir/$2" "$CurrDir/$2.fail.log"
	echo
	echo "Please check the logfile"
	echo
	echo "  $CurrDir/$2"
	echo
	echo "for errors. Last 20 lines:"
	echo
	tail -20 "$CurrDir/$2"
fi
exit $RetVal
