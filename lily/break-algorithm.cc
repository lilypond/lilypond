/*
  break.cc -- implement Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "paper-column.hh"
#include "break-algorithm.hh"
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
  Link_array<Paper_column> all (pscore_l_->col_l_arr_);
  Array<int> retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b ())
      retval.push (i);

  if (linewidth_f_ <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}


Link_array<Paper_column>
Break_algorithm::find_breaks () const
{
  Link_array<Paper_column> all (pscore_l_->col_l_arr_);
  Link_array<Paper_column> retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b ())
      retval.push (all[i]);

  if (linewidth_f_ <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}


Simple_spacer*
Break_algorithm::generate_spacing_problem (Link_array<Paper_column> curline, Interval line) const
{
  Simple_spacer * sp =  new Simple_spacer;
  Paper_def * d = pscore_l_->paper_l_;
  sp->compression_energy_factor_f_ = d->get_var ("compression_energy_factor");
  sp->default_space_f_ = d->get_var ("loose_column_distance");

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


  return sp;
}

Break_algorithm::Break_algorithm ()
{
  pscore_l_ = 0;
  linewidth_f_ = 0;
}

void
Break_algorithm::set_pscore (Paper_score*s)
{
  pscore_l_ = s;
  linewidth_f_ = s->paper_l_->get_var("linewidth");
  do_set_pscore ();
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
