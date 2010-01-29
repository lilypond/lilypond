/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "lily-guile.hh"
#include "dimensions.hh"
#include "warn.hh"

LY_DEFINE (ly_pt, "ly:pt",
	   1, 0, 0, (SCM num),
	   "@var{num} printer points.")
{
  LY_ASSERT_TYPE (scm_is_number, num, 1);
  return scm_from_double (point_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_cm, "ly:cm",
	   1, 0, 0, (SCM num),
	   "@var{num} cm.")
{
  LY_ASSERT_TYPE (scm_is_number, num, 1);
  return scm_from_double (cm_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_inch, "ly:inch",
	   1, 0, 0, (SCM num),
	   "@var{num} inches.")
{
  LY_ASSERT_TYPE (scm_is_number, num, 1);
  return scm_from_double (inch_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_mm, "ly:mm",
	   1, 0, 0, (SCM num),
	   "@var{num} mm.")
{
  LY_ASSERT_TYPE (scm_is_number, num, 1);
  return scm_from_double (mm_constant
			  * scm_to_double (num));
}

LY_DEFINE (ly_bp, "ly:bp",
	   1, 0, 0, (SCM num),
	   "@var{num} bigpoints (1/72th inch).")
{
  LY_ASSERT_TYPE (scm_is_number, num, 1);
  return scm_from_double (bigpoint_constant
			  * scm_to_double (num));
}
