# This file is part of LilyPond, the GNU music typesetter.
#
# Copyright (C) 2022--2023 Jean Abou Samra <jean@abou-samra.fr>
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

"""Parse toplevel VERSION file and provide its data in a `version_data` dictionary."""

from pathlib import Path
import re

version_file = Path(__file__).resolve().parent.parent.parent / "VERSION"
contents = version_file.read_text(encoding="utf-8")
contents = re.sub(r"#.*$", "", contents, flags=re.MULTILINE)  # trim comments
version_data = {}
for line in contents.splitlines():
    m = re.match(r"\s*(\S+)\s*=\s*(\S*)\s*", line)
    if m:
        key, value = m.groups()
        version_data[key] = value

_major = version_data["MAJOR_VERSION"]
_minor = version_data["MINOR_VERSION"]
_patch = version_data["PATCH_LEVEL"]
_my_patch = version_data["MY_PATCH_LEVEL"]
if _my_patch:
    version_data["TOPLEVEL_VERSION"] = f"{_major}.{_minor}.{_patch}.{_my_patch}"
else:
    version_data["TOPLEVEL_VERSION"] = f"{_major}.{_minor}.{_patch}"
