/*
  break.cc -- implement Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score-column.hh"
#include "break.hh"
#include "paper-def.hh"
#include "debug.hh"
#include "line-of-score.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "cpu-timer.hh"
#include "command-request.hh"
#include "simple-spacer.hh"





Array<int>
Break_algorithm::find_break_indices () const
{
  Line_of_cols all (pscore_l_->col_l_arr_);
  Array<int> retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b ())
      retval.push (i);

  if (linelength <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}


Line_of_cols
Break_algorithm::find_breaks () const
{
  Line_of_cols all (pscore_l_->col_l_arr_);
  Line_of_cols retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b ())
      retval.push (all[i]);

  if (linelength <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}


Line_spacer*
Break_algorithm::generate_spacing_problem (Line_of_cols curline, Interval line) const
{
  Line_spacer * sp =  new Simple_spacer;
  
  sp->default_space_f_ = pscore_l_->paper_l_->get_var ("loose_column_distance");

  sp->indent_f_ = line[LEFT];

  /*
    sort out how interfacing this should work;
   */
  if (line.empty_b())
    {
     sp->line_len_f_ = -1;
    }
  else
    sp->line_len_f_ = line.length ();
  
  sp->add_columns (curline);
  sp->prepare ();

  return sp;
}

Break_algorithm::Break_algorithm ()
{
  pscore_l_ = 0;
  get_line_spacer =0;
  linelength = 0;
}

void
Break_algorithm::set_pscore (Paper_score*s)
{
  pscore_l_ = s;
  linelength = s->paper_l_->linewidth_f ();
  do_set_pscore ();
}

bool
Break_algorithm::feasible (Line_of_cols curline) const
{
  if (linelength <=  0)
    return true;

  for (int i=0; i < curline.size (); i++)
    {
      if (i && i < curline.size () -1
	  && ((dynamic_cast<Score_column*>(curline[i]))->break_penalty_i () >= Break_req::FORCE))
	return false;
    }
  return true;
}

void
Break_algorithm::problem_OK () const
{
  if (pscore_l_->col_l_arr_.empty ())
    error (_("Score does not have any columns"));
  OK ();
}

void
Break_algorithm::OK () const
{
}

Array<Column_x_positions>
Break_algorithm::solve () const
{
  Array<Column_x_positions> h= do_solve ();
  
  return h;
}

void
Break_algorithm::do_set_pscore ()
{

}
