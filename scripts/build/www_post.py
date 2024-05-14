#!@PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2023 John Mandereau <john.mandereau@gmail.com>
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


# This is www_post.py. This script is the main stage
# of toplevel GNUmakefile local-WWW-post target.

# USAGE: www_post PACKAGE_NAME TOPLEVEL_VERSION OUTDIR TARGETS
# please call me from top of the source directory

import sys
import os
import re
import optparse

import postprocess_html

parser = optparse.OptionParser()
parser.add_option("--version", dest="version", action="store")
parser.add_option("--target", dest="target", action="store")
parser.add_option("--name", dest="name", action="store")

(options, args) = parser.parse_args()

target_dir = args[0]

if options.target not in ("online", "offline"):
    sys.stderr.write("target must be 'online' or 'offline'")
    os.exit(2)

# these redirection pages allow to go back to the documentation index
# from HTML manuals/snippets page
if os.path.isdir(os.path.join(target_dir, 'Documentation/')):
    for f, contents in {
        'index.html': '''<META HTTP-EQUIV="refresh" content="0;URL=Documentation/web/index.html">
    <html>
    <head>
    <title>Redirecting...</title>
    <meta name="author" content="This file was autogenerated">
    </head>
    <body>Redirecting to the documentation index...</body>
    </html>
    ''',
        'VERSION': options.version + '\n',
    }.items():
        open(os.path.join(target_dir, f), 'w',
             encoding='utf-8').write(contents)

    # need this for content negotiation with documentation index
    if options.target == 'online':
        f = open(os.path.join(target_dir, 'Documentation/.htaccess'), 'w', encoding='utf-8')
        f.write('#.htaccess\nDirectoryIndex index\n')
        f.close()

html_files = []
for root, dirs, files in os.walk(target_dir):
    for f in files:
        if f.endswith(".html"):
            html_files.append(os.path.join(root, f))

pages_dict = postprocess_html.build_pages_dict(html_files)
sys.stderr.write("Processing HTML pages for %s target...\n" % options.target)
postprocess_html.process_html_files(
    pages_dict,
    package_name=options.name,
    package_version=options.version,
    is_online=(options.target=="online"))
