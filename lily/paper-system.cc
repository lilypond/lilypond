/*
  paper-system.cc -- implement Paper_system

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-system.hh"
#include "stencil.hh"
#include "string.hh"
#include "virtual-methods.hh"

#include "ly-smobs.icc"

IMPLEMENT_SMOBS (Paper_system);
IMPLEMENT_TYPE_P (Paper_system, "ly:paper-system?");
IMPLEMENT_DEFAULT_EQUAL_P (Paper_system);



Paper_system::Paper_system (Stencil s, bool is_title)
{
  is_title_ = is_title;
  number_ = 0;
  penalty_ = 0;
  smobify_self ();
  stencil_ = s;
  staff_refpoints_ = Interval(0,0);
}

Paper_system::~Paper_system ()
{
}

SCM
Paper_system::mark_smob (SCM smob)
{
  Paper_system *system = (Paper_system*) SCM_CELL_WORD_1 (smob);
  return system-> stencil_.expr ();
}

int
Paper_system::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_system *p = (Paper_system*) SCM_CELL_WORD_1 (smob);
  scm_puts ("#<", port);
  scm_puts (classname (p), port);
  scm_puts (" ", port);
  scm_puts (to_string (p->number_).to_str0 (), port);
  scm_puts ("p ", port);
  scm_puts (to_string (p->penalty_).to_str0 (), port);
  if (p->is_title ())
    scm_puts (" t", port);
  scm_puts (" >", port);
  return 1;
}

bool
Paper_system::is_title () const
{
  return is_title_;
}

Real
Paper_system::penalty () const
{
  return penalty_;
}

Stencil
Paper_system::to_stencil () const
{
  return stencil_;
}

LY_DEFINE (ly_paper_system_height, "ly:paper-system-extent",
	   2, 0, 0, (SCM system, SCM axis),
	   "Return the extent of @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  SCM_ASSERT_TYPE (is_axis (axis), axis, SCM_ARG2, __FUNCTION__, "axis");
  Axis ax = (Axis)scm_to_int (axis);
  return ly_interval2scm (ps->to_stencil().extent (ax));
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
  return scm_int2num (ps->number_);
}

LY_DEFINE (ly_paper_system_break_score, "ly:paper-system-break-penalty",
	   1, 0, 0, (SCM system),
	   "Return the score for page break after @var{system}.")
{
  Paper_system *ps = unsmob_paper_system (system);
  SCM_ASSERT_TYPE (ps, system, SCM_ARG1, __FUNCTION__, "paper-system");
  return scm_int2num (int (ps->penalty ()));
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
  return ly_interval2scm (ps->staff_refpoints_);
}

