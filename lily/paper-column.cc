/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"
#include "axis-group-interface.hh"
#include "spaceable-grob.hh"

void
Paper_column::do_break_processing ()
{
  Spaceable_grob::remove_interface (this);
  Item::do_break_processing ();
}

int
Paper_column::rank_i (Grob*me) 
{
  return dynamic_cast<Paper_column*> (me)->rank_i_;
}

Line_of_score*
Paper_column::line_l () const
{
  return line_l_;
}

Paper_column*
Paper_column::column_l () const
{
  return (Paper_column*) (this);
}

Paper_column::Paper_column (SCM l)
  : Item (l)		// guh.?
{
  Axis_group_interface::set_interface (this);
  Axis_group_interface::set_axes (this, X_AXIS, X_AXIS);
  Spaceable_grob::set_interface (this);

  line_l_=0;
  rank_i_ = -1;
}

Moment
Paper_column::when_mom (Grob*me)
{
  SCM m = me->get_grob_property ("when");
  Moment s (0);
  if (unsmob_moment (m))
    {
      return *unsmob_moment (m);
    }
  return s;
}
  
bool
Paper_column::musical_b () const
{
  SCM m = get_grob_property ("shortest-starter-duration");
  Moment s (0);
  if (unsmob_moment (m))
    {
      s = *unsmob_moment (m);
    }
  return s != Moment (0);
}

bool
Paper_column::used_b (Grob*me)
{
  return gh_pair_p (me->get_grob_property ("elements")) ||  Item::breakable_b (me)
    || gh_pair_p (me->get_grob_property ("bounded-by-me"))
    ;
}
