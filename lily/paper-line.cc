/*
  paper-line.cc -- implement Paper_line

  source file of the GNU LilyPond music typesetter

  (c) 2004 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "paper-line.hh"
#include "ly-smobs.icc"

Paper_line::Paper_line (Offset o, SCM stencils, bool is_title)
{
  dim_ = o;
  stencils_ = stencils;
  is_title_ = is_title;
}

Offset
Paper_line::dim () const
{
  return dim_;
}

bool
Paper_line::is_title () const
{
  return is_title_;
}

SCM
Paper_line::stencils () const
{
  return stencils_;
}

SCM
Paper_line::mark_smob (SCM s)
{
  Paper_line *line = (Paper_line*) ly_cdr (s);
  return line->stencils_;
}

int
Paper_line::print_smob (SCM, SCM port, scm_print_state*)
{
  scm_puts ("#<Paper_line ", port);
  scm_puts (" >", port);
  return 1;
}

SCM
Paper_line::smobbed_copy () const
{
  Paper_line *line = new Paper_line (*this);
  return line->smobbed_self ();
}

IMPLEMENT_SIMPLE_SMOBS (Paper_line);
IMPLEMENT_TYPE_P (Paper_line, "ly:paper-line?");
IMPLEMENT_DEFAULT_EQUAL_P (Paper_line);
