#!@TARGET_PYTHON@
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022  Jonas Hahnfeld <hahnjo@hahnjo.de>
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

import gettext
import os
import re
import shlex
import subprocess
import sys
import urllib.parse

# Load translation and install _() into Python's builtins namespace.
gettext.install("lilypond", "@localedir@")


def show_version(file):
    file.write("lilypond-invoke-editor (GNU LilyPond @TOPLEVEL_VERSION@)\n")


def show_help(file):
    file.write(
        _(
            """Usage: lilypond-invoke-editor textedit://FILE:LINE:CHAR:COLUMN

Visit a file and position the cursor.

Options:
  -h, --help          show this help
  -v, --version       show version
"""
        )
    )


# We don't need heavy option parsing here, just expect exactly one argument.
if len(sys.argv) != 2:
    show_version(sys.stderr)
    show_help(sys.stderr)
    sys.exit(2)

argument = sys.argv[1]

# Handle the two options this script knows about.
if argument in ("-h", "--help"):
    show_version(sys.stdout)
    show_help(sys.stdout)
    sys.exit(0)
if argument in ("-v", "--version"):
    show_version(sys.stdout)
    sys.exit(0)

# Now start parsing the textedit argument, first by matching its components.
m = re.fullmatch(r"textedit://(.*):([0-9]+):([0-9]+):([0-9]*)", argument)
if m is None:
    show_help(sys.stderr)
    sys.exit(2)

file, line, char, column = m.groups()
file = urllib.parse.unquote(file)


def replace_template(template):
    return template % {
        "file": file,
        "line": line,
        "char": char,
        "column": column,
    }


# Determine the editor, going from more to less specific environment variables.
editor = "emacs"
for env in ("LYEDITOR", "XEDITOR", "EDITOR"):
    if env in os.environ:
        editor = os.environ[env]
        break

# Check if we have a template for this editor to position the cursor.
EDITOR_TEMPLATES = {
    "atom": [("atom", "%(file)s:%(line)s:%(column)s")],
    "emacs": [
        ("emacsclient", "--no-wait", "+%(line)s:%(column)s", "%(file)s"),
        ("emacs", "+%(line)s:%(column)s", "%(file)s"),
    ],
    "geany": [("geany", "--line", "%(line)s", "--column", "%(column)s", "%(file)s")],
    "gedit": [("gedit", "--wait", "%(file)s", "+%(line)s:%(column)s")],
    "gvim": [("gvim", "--remote", "+:%(line)s:norm%(column)s", "%(file)s")],
    "jedit": [("jedit", "-reuseview", "%(file)s", "+line:%(line)s")],
    "kate": [
        ("kate", "--block", "--line", "%(line)s", "--column", "%(column)s", "%(file)s")
    ],
    # "nc" nowadays also stands for netcat...
    "nedit": [("nc", "-noask", "+%(line)s", "%(file)s")],
    "syn": [("syn", "-line", "%(line)s", "-col", "%(char)s", "%(file)s")],
    "uedit32": [("uedit32", "%(file)s", "-l%(line)s", "-c%(char)s")],
}

editor_commands = []
if "%(file)s" in editor:
    editor_commands = [shlex.split(editor)]
elif editor in EDITOR_TEMPLATES:
    editor_commands = EDITOR_TEMPLATES[editor]
else:
    editor_commands = [(editor, "%(file)s")]

editor_commands = [list(map(replace_template, cmd)) for cmd in editor_commands]

# Try commands one after another, until one succeeds.
for cmd in editor_commands:
    try:
        proc = subprocess.run(cmd)
        if proc.returncode == 0:
            sys.exit(0)
    except FileNotFoundError as e:
        pass

sys.stderr.write(_("failed to invoke editor:"))
sys.stderr.write(" " + str(editor_commands) + "\n")
sys.exit(1)
