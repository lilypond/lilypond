/*
  paper-line.cc -- implement Paper_line

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-line.hh"
#include "stencil.hh"
#include "string.hh"
#include "virtual-methods.hh"

#define TITLE_PENALTY -1

Paper_line::Paper_line (Offset o, SCM stencils, int penalty, bool is_title)
{
  is_title_ = is_title;
  penalty_ = penalty;
  smobify_self ();


  /*
    TODO: we should extend the definition of stencil to allow

    stencil-expr:  LISTOF stencil-expr*

    so that we don't use as much memory for combining the stencils, and
    can do this conversion in constant time. 
  */
  for (SCM s = stencils; ly_c_pair_p (s); s = ly_cdr (s))
    stencil_.add_stencil (*unsmob_stencil (ly_car (s)));

  Box x (Interval (0, o[X_AXIS]), Interval (0, o[Y_AXIS]));
  stencil_ = Stencil (x, stencil_.expr ());
}

Paper_line::~Paper_line ()
{
}

#include "ly-smobs.icc"
IMPLEMENT_SMOBS (Paper_line);
IMPLEMENT_TYPE_P (Paper_line, "ly:paper-line?");
IMPLEMENT_DEFAULT_EQUAL_P (Paper_line);

SCM
Paper_line::mark_smob (SCM smob)
{
  Paper_line *line = (Paper_line*) ly_cdr (smob);
  return line-> stencil_.expr ();
}

int
Paper_line::print_smob (SCM smob, SCM port, scm_print_state*)
{
  Paper_line *p = (Paper_line*) ly_cdr (smob);
  scm_puts ("#<", port);
  scm_puts (classname (p), port);
  scm_puts (to_string (p->number_).to_str0 (), port);
  if (p->is_title ())
    scm_puts (" t", port);
  scm_puts (" >", port);
  return 1;
}

bool
Paper_line::is_title () const
{
  return is_title_;
}

int
Paper_line::penalty () const
{
  return penalty_;
}

Offset
Paper_line::dim () const
{
  return Offset (stencil_.extent (X_AXIS).length (),
		 stencil_.extent (Y_AXIS).length ());
}

Stencil
Paper_line::to_stencil () const
{
  return stencil_;
}

LY_DEFINE (ly_paper_line_height, "ly:paper-line-height",
	   1, 0, 0, (SCM line),
	   "Return the height of @var{line}.")
{
  Paper_line *pl = unsmob_paper_line (line);
  SCM_ASSERT_TYPE (pl, line, SCM_ARG1, __FUNCTION__, "paper-line");
  return scm_make_real (pl->dim ()[Y_AXIS]);
}

LY_DEFINE (ly_paper_line_number, "ly:paper-line-number",
	   1, 0, 0, (SCM line),
	   "Return the number of @var{line}.")
{
  Paper_line *pl = unsmob_paper_line (line);
  SCM_ASSERT_TYPE (pl, line, SCM_ARG1, __FUNCTION__, "paper-line");
  return scm_int2num (pl->number_);
}

LY_DEFINE (ly_paper_line_break_score, "ly:paper-line-break-score",
	   1, 0, 0, (SCM line),
	   "Return the score for page break after @var{line}.")
{
  Paper_line *pl = unsmob_paper_line (line);
  SCM_ASSERT_TYPE (pl, line, SCM_ARG1, __FUNCTION__, "paper-line");
  return scm_int2num (int (pl->penalty ()));
}

LY_DEFINE (ly_paper_line_stencil, "ly:paper-line-stencil",
	   1, 0, 0, (SCM line),
	   "Return the height of @var{line}.")
{
  Paper_line *pl = unsmob_paper_line (line);
  SCM_ASSERT_TYPE (pl, line, SCM_ARG1, __FUNCTION__, "paper-line");
  return pl->to_stencil ().smobbed_copy ();
}

