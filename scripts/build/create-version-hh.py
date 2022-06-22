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

"""Generate `version.hh` from `VERSION` to make version data available in C++."""

from version_data import version_data

print("""\
// Automatically generated from VERSION by create-version-hh.py

#ifndef VERSION_HH
#define VERSION_HH
""")
for key, value in version_data.items():
    # Guile leaks Autoconf data into userspace (TODO: is this still true?)
    print(f"""
#ifdef {key}
#undef {key}
#endif /* {key} */
#define {key} "{value}"
""")

print("""
#endif /* VERSION_HH */
""")
