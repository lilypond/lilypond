# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022 Jean Abou Samra <jean@abou-samra.fr>
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

"""Update the Pygments archive in ``python/auxiliar/vendored``.

Download the archive from the Web, and trim it by removing unnecessary
files to reduce its size.
"""

import argparse
import io
import os
import pathlib
from typing import Optional
import zipfile

import requests

here = pathlib.Path(__file__)
root = here.parent.parent.parent
dest = root / "python" / "auxiliar" / "vendored" / "pygments.zip"

p = argparse.ArgumentParser()
p.add_argument("rev", help="Git revision in the Pygments repository")
args = p.parse_args()

keep_paths = {
    "lexers": (
        "__init__",
        "lilypond",
        "_lilypond_builtins",
        "lisp",
        "_scheme_builtins",
        "python",
        "_mapping",
    ),
    "formatters": ("__init__", "html", "_mapping"),
    "styles": ("default", "__init__"),
    "filters": ("__init__",),
}

archive_root = f"pygments-{args.rev}"


def process(filename: str) -> Optional[str]:
    """Return ``None`` if ``filename`` should be removed, else a new path for it."""
    path = pathlib.PurePosixPath(filename)
    rel = path.relative_to(archive_root)
    try:
        subrel = rel.relative_to("pygments/")
    except ValueError:
        # Outside of pygments/ are test files, LICENSE, MANIFEST.in, ...
        return None
    # Outside of pygments/ subdirectories are fundamental files.
    if len(subrel.parts) != 2:
        return str(rel)
    subdir = subrel.parts[0]
    if rel.stem in keep_paths[subdir]:
        return str(rel)
    return None


r = requests.get(f"https://github.com/pygments/pygments/archive/{args.rev}.zip")
with zipfile.ZipFile(io.BytesIO(r.content)) as in_file:
    with zipfile.ZipFile(dest, "w", compression=zipfile.ZIP_DEFLATED) as out_file:
        for info in in_file.infolist():
            if not info.is_dir():
                newname = process(info.filename)
                if newname is not None:
                    out_file.writestr(newname, in_file.read(info.filename))
