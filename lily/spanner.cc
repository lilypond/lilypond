/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "spanner.hh"
#include "p-col.hh"
#include "p-score.hh"
#include "molecule.hh"
#include "paper-outputter.hh"



void
Spanner::do_print() const
{
#ifndef NPRINT
  DOUT << "Between " << classname (spanned_drul_[LEFT])
       << " and " << classname (spanned_drul_[RIGHT]) << '\n';

  if (broken_b ())
    DOUT << "Broken in " << to_str (broken_info_.size ()) << " pieces";
#endif
}

void
Spanner::break_into_pieces ()
{
  if (broken_b ())
    return; 
	 
  Item * left = spanned_drul_[LEFT];
  Item * right = spanned_drul_[RIGHT];
  
  if  (left == right)
    {
      warning (_ ("left spanpoint is right spanpoint\n"));
      return;
    }
  
  Link_array<Item> break_points = pscore_l_->broken_col_range (left,right);
  Link_array<Spanner> broken_into_l_arr;

  break_points.insert (left,0);
  break_points.push (right);


  for (int i=1; i < break_points.size(); i++) 
    {
      Breaking_information info;
      info.bounds_[LEFT] = break_points[i-1];
      info.bounds_[RIGHT] = break_points[i];
      Direction d = LEFT;
      do
	{
	  Item *&pc_l = info.bounds_[d] ;
	  if (!pc_l->line_l())
	    pc_l =  pc_l->find_prebroken_piece(- d);

	  assert (pc_l);
	  if (!info.line_l_)
	    info.line_l_ = pc_l-> line_l ();
	  else
	    assert( info.line_l_ = pc_l->line_l ());
	  
	}
      while ((flip(&d))!= LEFT);
      info.broken_spanner_l_ = 0;
      broken_info_.push (info);
    }
}

void
Spanner::set_my_columns()
{
  Direction i = (Direction)1;
  do 
    {
      if (!spanned_drul_[i]->line_l())
	set_bounds(i,spanned_drul_[i]->find_prebroken_piece((Direction)-i));
    } 
  while (flip(&i) != 1);
}       


void
Spanner::set_bounds(Direction d, Item*i)
{
  spanned_drul_[d] =i;
  if (i)
    {
      i->used_b_ = true;
    }
  
  if  (spanned_drul_[Direction(-d)] == spanned_drul_[d]
       && i)
    warning (_f ("Spanner `%s\' with equal left and right spanpoints", classname (this)));
}

void
Spanner::do_break_processing()
{
  if (!line_l())
    break_into_pieces ();
  else 
    handle_broken_dependencies();
}

Spanner::Spanner ()
{
  spanned_drul_[LEFT]=0;
  spanned_drul_[RIGHT]=0;
}

Spanner::Spanner (Spanner const &s)
  :Score_element (s)
{
  spanned_drul_[LEFT] = spanned_drul_[RIGHT] =0;
}

void
Spanner::output_processing () 
{
  if (get_elt_property (transparent_scm_sym) != SCM_BOOL_F)
    return;

  output_p_ = do_brew_molecule_p ();
  Offset left_off (spanned_drul_[LEFT]->absolute_coordinate(X_AXIS), 0);
  Offset o = absolute_offset() + left_off;
  pscore_l_->outputter_l_->output_molecule (output_p_, o, classname (this));
}

Interval
Spanner::do_width() const
{  
  Real l = spanned_drul_[LEFT]->absolute_coordinate (X_AXIS);
  Real r = spanned_drul_[RIGHT]->absolute_coordinate (X_AXIS);

  if (r< l)
    warning ("Spanner with negative length");
	
  return Interval (0, r-l);
}

Line_of_score *
Spanner::line_l() const
{
  if (!spanned_drul_[LEFT] || !spanned_drul_[RIGHT])
    return 0;
  if (spanned_drul_[LEFT]->line_l() != spanned_drul_[RIGHT]->line_l())
    return 0;
  return spanned_drul_[LEFT]->line_l();
}


Spanner*
Spanner::find_broken_piece (Line_of_score*l) const
{
  for (int i=0; i < broken_info_.size (); i++)
    {
      Spanner *me =(Spanner*) this;
      Breaking_information &info  = me->broken_info_[i];
      if (info.line_l_ == l)
	{
	  if (!info.broken_spanner_l_)
	    {
	      Spanner *span_p = dynamic_cast<Spanner*>(clone ());
	      span_p->set_bounds(LEFT,info.bounds_[LEFT]);
	      span_p->set_bounds(RIGHT,info.bounds_[RIGHT]);
	      pscore_l_->typeset_element (span_p);

	      
	      info.broken_spanner_l_ = span_p;
	      span_p->handle_broken_dependencies();

	    }
	  return info.broken_spanner_l_;
	}
    }

  return 0;				   
}

bool
Spanner::broken_b() const
{
  return broken_info_.size();
}




Array<Rod>
Spanner::get_rods () const
{
  Array<Rod> r;
  return r;
}

void
Spanner::do_space_processing ()
{
  Array<Rod> rs (get_rods ());
  for (int i=0; i < rs.size (); i++)
    {
      rs[i].add_to_cols ();
    }
}
