/*
  break.cc -- implement Break_algorithm

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "score-column.hh"
#include "break.hh"
#include "paper-def.hh"
#include "line-spacer.hh"
#include "debug.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "p-col.hh"
#include "cpu-timer.hh"

String
Col_stats::str () const {
  String s (count_i_);
  s += _ (" lines");
  if  (count_i_)
    s += String (Real (cols_i_)/count_i_, _(", (with an average of %.1f columns)"));

  return s;
}

void
Col_stats::add (Line_of_cols const& line)
{
  count_i_++;
  cols_i_ += line.size ();
}


Col_stats::Col_stats ()
{
  count_i_ =0;
  cols_i_ =0;
}

/* **************************************************************** */

Line_of_cols
Break_algorithm::all_cols () const
{
  Line_of_cols retval;
  for (PCursor<Paper_column*> c (pscore_l_->col_p_list_.top ());
       c.ok (); c++)
    {

      retval.push (c);
    }
  return retval;
}

Array<int>
Break_algorithm::find_break_indices () const
{
  Line_of_cols all (all_cols ());
  Array<int> retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b_)
      retval.push (i);

  if (linelength <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}

///  return all breakable columns
Line_of_cols
Break_algorithm::find_breaks () const
{
  Line_of_cols all (all_cols ());
  Line_of_cols retval;

  for (int i=0; i < all.size (); i++)
    if (all[i]->breakable_b_)
      retval.push (all[i]);


  if (linelength <=0)
    while (retval.size () >2)
      retval.del (1);

  return retval;
}





Line_spacer*
Break_algorithm::generate_spacing_problem (Line_of_cols curline, Interval line) const
{
  Line_spacer * sp= (*get_line_spacer) ();

  sp->paper_l_ = pscore_l_->paper_l_;
  sp->add_column (curline[0], true, line.min ());
  for (int i=1; i< curline.size ()-1; i++)
    sp->add_column (curline[i]);

  if (line.length () > 0)
    sp->add_column (curline.top (), true, line.max ());
  else
    sp->add_column (curline.top ());

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
	  && ((Score_column*)curline[i])->forced_break_b ())
	return false;
    }
  return true;
}

void
Break_algorithm::problem_OK () const
{
  if (!pscore_l_->col_p_list_.size ())
    error (_("Score does not have any columns"));
  OK ();
}

void
Break_algorithm::OK () const
{
#ifndef NDEBUG
  iter_top (pscore_l_->col_p_list_,start);
  PCursor<Paper_column *> end (pscore_l_->col_p_list_.bottom ());

  assert (start->breakable_b_);
  assert (end->breakable_b_);
#endif
}

Array<Col_hpositions>
Break_algorithm::solve () const
{
  Cpu_timer timer;

  Array<Col_hpositions> h= do_solve ();

  if (approx_stats_.count_i_)
    *mlog << _ ("\nApproximated: ") << approx_stats_.str () << "\n";
  if (exact_stats_.count_i_)
    *mlog << _ ("Calculated exactly: ") << exact_stats_.str () << "\n";
  *mlog << _ ("Time: ") << String (timer.read (), "%.2f") << _ (" seconds\n");

  return h;
}

void
Break_algorithm::do_set_pscore ()
{

}
