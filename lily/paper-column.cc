/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"
#include "axis-group-interface.hh"

void
Paper_column::add_rod (Paper_column * p, Real d)
{
  Direction dir =  Direction (sign (p->rank_i ()  - rank_i ()));
  
  if (dir != RIGHT)
    {
      programming_error ("Must set minimum distance LTOR.");
      return;
    }
  
  for (int i=0; i < minimal_dists_.size (); i++)
    {
      Column_rod &rod = minimal_dists_[i];
      if (rod.other_l_ == p)
	{
	  rod.distance_f_ = rod.distance_f_ >? d;
	  return ;
	}
    }

  Column_rod cr;
  cr.distance_f_ = d;
  cr.other_l_ = p;

  minimal_dists_.push (cr);
}

void
Paper_column::add_spring (Paper_column * p, Real d, Real s)
{
  Direction dir =  Direction (sign (p->rank_i ()  - rank_i ()));
  
  if (dir != RIGHT)
    {
      programming_error ("Must set springs LTOR");
      return;
    }
  
  for (int i=0; i < springs_.size (); i++)
    {
      Column_spring &spring = springs_[i];
      if (spring.other_l_ == p)
	{
	  spring.distance_f_ = spring.distance_f_ >? d;
	  return ;
	}
    }

  Column_spring cr;
  cr.distance_f_ = d;
  cr.strength_f_ = s;  
  cr.other_l_ = p;

  springs_.push (cr);
}

int
Paper_column::rank_i() const
{
  return rank_i_;
}

void
Paper_column::set_rank (int i)
{
  rank_i_ = i;
}



Line_of_score*
Paper_column::line_l() const
{
  return line_l_;
}

Paper_column*
Paper_column::column_l () const
{
  return (Paper_column*)(this);
}

Paper_column::Paper_column (SCM l)
  : Item (l)		// guh.?
{
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (X_AXIS, X_AXIS);
  set_elt_pointer ("bounded-by-me", SCM_EOL);
  line_l_=0;
  rank_i_ = -1;
}

Moment
Paper_column::when_mom () const
{
  SCM m = get_elt_property ("when");
  Moment s (0);
  if (SMOB_IS_TYPE_B(Moment, m))
    {
      s = *SMOB_TO_TYPE (Moment,m);
    }
  return s;
}
  
bool
Paper_column::musical_b () const
{
  SCM m = get_elt_property ("shortest-starter-duration");
  Moment s (0);
  if (SMOB_IS_TYPE_B(Moment, m))
    {
      s = *SMOB_TO_TYPE (Moment,m);
    }
  return s != Moment(0);
}

bool
Paper_column::used_b ()const
{
  return gh_pair_p (get_elt_pointer ("elements")) ||  breakable_b ()
    || gh_pair_p (get_elt_pointer ("bounded-by-me"))
    ;
}
