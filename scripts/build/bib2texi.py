# bib2texi.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
#               2022 Jean Abou Samra <jean@abou-samra.fr>
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

from argparse import ArgumentParser
from pathlib import Path
from os import environ
from shutil import copy
from subprocess import PIPE, run
from tempfile import TemporaryDirectory

p = ArgumentParser()
p.add_argument("-s", "--style")
p.add_argument("-o", "--output")
p.add_argument("file")
args = p.parse_args()

with TemporaryDirectory() as tempdir:
    bibtex_input = Path(args.file).with_suffix("")
    tempfile = Path(tempdir) / "bib2texi-tmp.aux"
    tempfile.write_text(rf"""
\relax
\citation{{*}}
\bibstyle{{{args.style}}}
\bibdata{{{bibtex_input}}}
""",
                       encoding="utf-8")
    completed = run(["bibtex", "-terse", tempfile],
                    env=dict(environ, TEXMFOUTPUT=tempdir),
                    stdout=PIPE,
                    encoding="utf-8")
    if completed.returncode:
        print(f"bibtex exited with return code {completed.returncode}.  Output:")
        print(completed.stdout)
        raise SystemExit
    copy(Path(tempdir) / "bib2texi-tmp.bbl", args.output)
