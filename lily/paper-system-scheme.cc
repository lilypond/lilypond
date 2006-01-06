/*
  paper-system-scheme.cc -- implement Paper_system bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "paper-system.hh"

LY_DEFINE (ly_paper_system_set_property_x, "ly:paper-system-set-property!",
	   2, 1, 0, (SCM system, SCM sym, SCM value),
	   "Set property @var{sym} of @var{system} to @var{value}")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");

  ps->internal_set_property (sym, value);
  return SCM_UNSPECIFIED;
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


