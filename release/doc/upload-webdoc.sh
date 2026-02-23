#!/bin/sh

# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2026 Jonas Hahnfeld <hahnjo@hahnjo.de>
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

if [ $# -lt 1 ]; then
  echo "Too few arguments, pass the LilyPond version number" >&2
  exit 1
fi
version="$1"
web_ball="lilypond-$version-webdoc.tar.xz"
server="graham@gcp.lilypond.org"
doc="/var/www/lilypond/doc"
version_dir="v$(echo "$version" | cut -d '.' -f 1,2)"
date_dir="$version_dir.$(date +%Y%m%d)"

if [ ! -f "$web_ball" ]; then
  echo "Could not find $web_ball" >&2
  exit 1
fi

# Create a script to extract the documentation.
ssh "$server" sh -c "cat > extract.sh" <<EOF
# Fail on errors.
set -e

if [ -e "$doc/tmp" ]; then
  echo "$doc/tmp already exists" >&2
  exit 1
fi

mkdir $doc/tmp
cd $doc/tmp

echo "Extracting web documentation..."
tar xJvf -

cd $doc
if [ -e "$doc/$version_dir" ]; then
  mv $version_dir $date_dir
  echo "$doc/$version_dir moved to $doc/$date_dir, please clean up afterards"
fi
mv tmp $version_dir
EOF
if [ $? -ne 0 ]; then
  echo "Failed to create script to extract the documentation" >&2
  exit 1
fi

cat "$web_ball" | ssh "$server" sh extract.sh
if [ $? -ne 0 ]; then
  echo "Failed to extract the documentation" >&2
  exit 1
fi

exit 0
