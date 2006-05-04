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

vector<Grob*>
Break_algorithm::find_breaks () const
{
  vector<Grob*> all = pscore_->root_system ()->columns ();
  vector<Grob*> retval;

  for (vsize i = 0; i < all.size (); i++)
    {
      Item *it = dynamic_cast<Item*> (all[i]);
      if (Item::is_breakable (all[i])
	  && (i == 0 || it->find_prebroken_piece (LEFT))
	  && (i == all.size () - 1 || it->find_prebroken_piece (RIGHT)))
	retval.push_back (all[i]);
    }

  return retval;
}

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
