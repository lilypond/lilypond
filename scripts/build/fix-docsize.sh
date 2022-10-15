#!/bin/sh
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2020--2022  David Kastrup <dak@gnu.org>
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


# Call with a number of HTML files as argument that may contain
# comments of the form
#
# <!-- file-size:, href="dir/filename.ext"-->
#
# Those are going to be replaced (in this spelling) with a comma and
# space, followed by the actual human-readable file size indicating
# the size of the file for download.  The somewhat peculiar syntax is
# chosen so that python/auxiliar/postprocess_html.py also does its
# path processing on the size comments.

# Only treat files actually having file-size source comments
sourcefiles=$(grep -l '<!-- file-size:' "$@")

# get out if there are no such files
[ -z "$sourcefiles" ] && exit 0

# Get list of referenced files and uniquify
files=$(sed -n '/<!-- file-size:/s/^.*<!-- file-size:[^>]*href="\([^"]*\)".*$/\1/p' $sourcefiles | sort -u)

# We build an sed command having one replacement for each
# unique referenced file in a get-file-size comment.

# Now build script over files accessible from cwd
files=$(for filename in $files
	do
	    if ! [ -r "$filename" ] || ! [ -f "$filename" ]
	    then
		# If a file cannot be found from multiple places, we
		# list those as a braced list, otherwise just as a
		# single file
		printf '%s: %s not accessible from %s/%s\n' \
		       "$(basename $0)" "$filename" "$(pwd)" \
		       "$(grep -l "<!-- file-size:[^>]*href=\"$filename\"" $sourcefiles |
			     sed -n 's|^\./||
			             1h
				     1!H
				     ${x
				       /\n/{s/^/{/
				            s/\n/,/g
					    s/$/}/
					   }
				       p}')" >&2
	    else
		echo "$filename"
	    fi
	done)

# Only do the replacement if there are actually accessible files
# mentioned in the comments.
[ -z "$files" ] && exit 0

# echo "substituting size for:" $files >&2

script=$(ls -sLh $files |
	     sed 's/^[ 	]*\([^ 	]*\)[ 	]*\([^ 	]*\)[ 	]*$/'\
'\/<!-- file-size:\/s|<!-- file-size:\\([^>]*\\)href="\2"\\([^>]*\\)-->|\\1\1\\2|/')

# Using the same script for every file is efficient but it means that
# every file here gets touched, even those containing only references
# to inaccessible files.  If only such references remain in all files,
# however, we don't touch anything.
#
# That's sort of good enough.

sed -i -e "$script" $sourcefiles
