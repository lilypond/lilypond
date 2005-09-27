/*
  paper-system-scheme.cc -- implement Paper_system bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-system.hh"

LY_DEFINE (ly_paper_system_height, "ly:paper-system-extent",
	   2, 0, 0, (SCM system, SCM axis),
	   "Return the extent of @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  Axis ax = (Axis)scm_to_int (axis);
  return ly_interval2scm (ps->to_stencil ().extent (ax));
}

LY_DEFINE (ly_paper_system_title_p, "ly:paper-system-title?",
	   1, 0, 0, (SCM system),
	   "Is  @var{system} a title system?")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return SCM_BOOL (ps->is_title ());
}

LY_DEFINE (ly_paper_system_number, "ly:paper-system-number",
	   1, 0, 0, (SCM system),
	   "Return the number of @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return scm_from_int (ps->number_);
}

LY_DEFINE (ly_paper_system_break_before_penalty, "ly:paper-system-break-before-penalty",
	   1, 0, 0, (SCM system),
	   "Return the score for page break after @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return scm_from_int (int (ps->break_before_penalty ()));
}

LY_DEFINE (ly_paper_system_stencil, "ly:paper-system-stencil",
	   1, 0, 0, (SCM system),
	   "Return the height of @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return ps->to_stencil ().smobbed_copy ();
}

LY_DEFINE (ly_paper_system_staff_extent, "ly:paper-system-staff-extents",
	   1, 0, 0, (SCM system),
	   "Return the top and bottom staff refpoint.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return ly_interval2scm (ps->staff_refpoints ());
}



LY_DEFINE (ly_paper_system_property, "ly:paper-system-property",
	   2, 1, 0, (SCM system, SCM sym, SCM dfault),
	   "Return the value for @var{sym}. Properties may be set by "
	   "setting the @code{line-break-system-details} property of "
	   "NonMusicalPaperColumn.  If the property is not found, "
	   "return @var{dfault}, "
	   "or @code{'()} if undefined.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  if (dfault == SCM_UNDEFINED)
    dfault = SCM_EOL;

  SCM retval = ps->internal_get_property (sym);
  if (retval == SCM_EOL)
    return dfault;
  else
    return retval;
}


