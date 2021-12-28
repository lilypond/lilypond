/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LIBC_EXTENSION_HH
#define LIBC_EXTENSION_HH

// This differs from std::round() in the handling of negative numbers:
// While std::round() rounds halfway cases away from zero, this function
// rounds them up (for historic reasons). As an example,
//   std::round(-7.5) = -8.0
//   round_halfway_up(-7.5) = -7.0
//
// This can apparently be relied upon at least when manually positioning
// ties (input/regression/tie-single-manual.ly). In the long run, these
// cases should be identified and taken care of individually. Keep this
// function (old name: my_round) for the time being, but DO NOT USE in
// newly written code.
double round_halfway_up (double);

#endif /* LIBC_EXTENSION_HH */
