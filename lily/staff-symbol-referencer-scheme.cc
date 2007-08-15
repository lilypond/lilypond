/*
  staff-symbol-referencer-scheme.cc -- implement Staff_symbol_referencer bindings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "libc-extension.hh"

LY_DEFINE (ly_grob_staff_position, "ly:grob-staff-position",
	   1, 0, 0, (SCM sg),
	   "Return the Y-position of @var{sg} relative to the staff.")
{
  Grob *g = unsmob_grob (sg);

  SCM_ASSERT_TYPE (g, sg, SCM_ARG1, __FUNCTION__, "grob");
  Real pos = Staff_symbol_referencer::get_position (g);

  if (fabs (rint (pos) -pos) < 1e-6) // ugh.
    return scm_from_int ((int) my_round (pos));
  else
    return scm_from_double (pos);
}
