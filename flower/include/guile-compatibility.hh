/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef GUILE_COMPATIBILITY_HH
#define GUILE_COMPATIBILITY_HH

#if SCM_MAJOR_VERSION == 1
#if SCM_MINOR_VERSION > 6 && SCM_MINOR_VERSION < 9
/*
  GUILE V1.7.0 - V1.8.n
*/
#define scm_from_unsigned(x) scm_from_unsigned_integer (x)
#else  // SCM_MINOR_VERSION >= 9
/*
  GUILE V1.9.n
*/
#endif // SCM_MINOR_VERSION > 6 && SCM_MINOR_VERSION < 9
#else  // SCM_MAJOR_VERSION != 1
/*
   Add any compatibility definitions here for Guile V2.n
*/
#endif // SCM_MAJOR_VERSION == 1
#if defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION > 1)
#define GUILEV2 1
#endif
// TODO - remove GUILE1 definition when support for Guile 1 is dropped.
#if defined (SCM_MAJOR_VERSION) && (SCM_MAJOR_VERSION < 2)
#define GUILEV1 1
#define GUILEV2 0
#endif
#endif /* GUILE_COMPATIBILITY_HH */
