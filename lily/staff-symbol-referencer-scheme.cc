/*
  staff-symbol-referencer-scheme.cc -- implement Staff_symbol_referencer bindings

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "staff-symbol.hh"
#include "libc-extension.hh"

LY_DEFINE (ly_grob_staff_position, "ly:grob-staff-position",
	   1, 0, 0, (SCM sg),
	   "Return the Y-position of @var{sg} relative to the staff.")
{
  LY_ASSERT_SMOB (Grob, sg, 1);
  Grob *g = unsmob_grob (sg);
  Real pos = Staff_symbol_referencer::get_position (g);

  if (fabs (rint (pos) -pos) < 1e-6) // ugh.
    return scm_from_int ((int) my_round (pos));
  else
    return scm_from_double (pos);
}

LY_DEFINE (ly_position_on_line_p, "ly:position-on-line?",
           2, 0, 0, (SCM sg, SCM spos),
           "Return whether @var{pos} is on a line of the staff associated with the the grob @var{sg} (even on an extender line).")
{
  LY_ASSERT_SMOB (Grob, sg, 1);
  LY_ASSERT_TYPE (scm_is_number, spos, 2);
  Grob *g = unsmob_grob (sg);
  Grob *st = Staff_symbol_referencer::get_staff_symbol (g);
  int pos = scm_to_int (spos);
  bool on_line = st ? Staff_symbol::on_line (g, pos) : false;
  return scm_from_bool (on_line);
}
