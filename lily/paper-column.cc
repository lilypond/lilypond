/*
  paper-column.cc -- implement Paper_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "debug.hh"

void
Paper_column::add_rod (Paper_column * p, Real d)
{
  Direction dir =  Direction (sign (p->rank_i ()  - rank_i ()));
  
  if (!dir)
    {
      programming_error ("Must set minimum distance between differing columns.");
      return;
    }
  
  for (int i=0; i < minimal_dists_arr_drul_[dir].size (); i++)
    {
      Column_rod &rod = minimal_dists_arr_drul_[dir][i];
      if (rod.other_l_ == p)
	{
	  rod.distance_f_ = rod.distance_f_ >? d;
	  return ;
	}
    }

  Column_rod cr;
  cr.distance_f_ = d;
  cr.other_l_ = p;

  minimal_dists_arr_drul_[dir].push (cr);
}

void
Paper_column::add_spring (Paper_column * p, Real d, Real s)
{
  Direction dir =  Direction (sign (p->rank_i ()  - rank_i ()));
  
  if (!dir)
    {
      warning (_ ("Must set spring between differing columns"));
      return;
    }
  
  for (int i=0; i < spring_arr_drul_[dir].size (); i++)
    {
      Column_spring &spring = spring_arr_drul_[dir][i];
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

  spring_arr_drul_[dir].push (cr);
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

void
Paper_column::do_print() const
{
#ifndef NPRINT
  DEBUG_OUT << "rank: " << rank_i_ << '\n';
  Direction d = LEFT;
  do
    {
      for (int i=0; i < minimal_dists_arr_drul_[d].size (); i++)
	{
	  minimal_dists_arr_drul_[d][i].print ();
	}
      for (int i=0; i < spring_arr_drul_[d].size (); i++)
	{
	  spring_arr_drul_[d][i].print ();
	}
      
    }
  while ((flip (&d))!=LEFT);
  Item::do_print ();
#endif 
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




Paper_column::Paper_column (Moment w)
{
  SCM when = (new Moment (w))->smobify_self ();;
  scm_unprotect_object (when);
  set_elt_property ("when", when);
  
  set_axes (X_AXIS, X_AXIS);

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
