/*
  break.cc -- implement Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "break-algorithm.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "system.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "cpu-timer.hh"
#include "simple-spacer.hh"

Break_algorithm::Break_algorithm ()
{
  pscore_ = 0;
  linewidth_ = 0;
}

void
Break_algorithm::set_pscore (Paper_score *s)
{
  pscore_ = s;
  linewidth_ = s->layout ()->get_dimension (ly_symbol2scm ("line-width"));
}

vector<Column_x_positions>
Break_algorithm::solve () 
{
  vector<Column_x_positions> h;
  return h;
}

Break_algorithm::~Break_algorithm ()
{
}
