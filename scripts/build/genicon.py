# genicon.py
#
# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2006--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
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

import argparse
from pathlib import Path
import subprocess
import tempfile

p = argparse.ArgumentParser(description="generate Windows icon files from xpm images")
p.add_argument("input")
p.add_argument("output")
args = p.parse_args()

with tempfile.TemporaryDirectory() as tempdir:
    pngs = []
    for size in ["16", "32", "48"]:
        for depth in ["8", "24"]:
            png = Path(tempdir) / f"icon-{size}-{depth}.png"
            cmd_args = ["convert", "-depth", depth, "-sample", size, args.input, png]
            subprocess.run(cmd_args, encoding="utf-8", check=True)
            pngs.append(png)
    cmd_args = ["icotool", "--output", args.output, "--create", *pngs]
    subprocess.run(cmd_args, encoding="utf-8", check=True)
