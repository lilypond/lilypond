/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dimension-cache.hh"
#include "debug.hh"
#include "spanner.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "molecule.hh"
#include "paper-outputter.hh"
#include "score-column.hh"
#include "line-of-score.hh"

void
Spanner::do_print() const
{
#ifndef NPRINT
  DOUT << "Between " << classname (spanned_drul_[LEFT])
       << " and " << classname (spanned_drul_[RIGHT]) << '\n';

  if (broken_b ())
    DOUT << "Broken in " << to_str (broken_into_l_arr_.size ()) << " pieces";
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

      Spanner *span_p = dynamic_cast<Spanner*>(clone ());
      span_p->set_bounds(LEFT,info.bounds_[LEFT]);
      span_p->set_bounds(RIGHT,info.bounds_[RIGHT]);
      pscore_l_->typeset_element (span_p);

      info.broken_spanner_l_ = span_p;
      span_p->handle_broken_dependencies();

      broken_into_l_arr_.push (span_p);
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

  if (d== LEFT)
    {
      set_parent ( i, X_AXIS);
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
  : Score_element (s)
{
  spanned_drul_[LEFT] = spanned_drul_[RIGHT] =0;
}


Interval
Spanner::do_width() const
{  
  Real l = spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS);
  Real r = spanned_drul_[RIGHT]->relative_coordinate (0, X_AXIS);

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
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      if (broken_into_l_arr_[i]->line_l () == l)
	return broken_into_l_arr_[i];
    }

  return 0;				   
}

bool
Spanner::broken_b() const
{
  return broken_into_l_arr_.size();
}

Array<Rod>
Spanner::get_rods () const
{
  Array<Rod> r;
  return r;
}

Array<Spring>
Spanner::get_springs () const
{
  Array<Spring> s;
  return s;    
}

void
Spanner::do_space_processing ()
{
  Array<Rod> rs (get_rods ());
  for (int i=0; i < rs.size (); i++)
    {
      rs[i].add_to_cols ();
    }

  Array<Spring> ss (get_springs ());
  for (int i=0; i < ss.size (); i++)
    {
      ss[i].add_to_cols ();
    }
}

/*
  UGH.
 */
void
Spanner::handle_broken_dependents ()
{
  Spanner *unbrok = dynamic_cast<Spanner*> (original_l_);
  if (!unbrok || parent_l(Y_AXIS))
    return;
  
  Spanner *refpoint = dynamic_cast<Spanner*> (unbrok->parent_l (Y_AXIS));
  
  if (refpoint)
    {
      Spanner * broken_refpoint = refpoint->find_broken_piece (line_l ());
      if (broken_refpoint)
	set_parent ( broken_refpoint,Y_AXIS);
      else
	programming_error ("Spanner y -refpoint lost.");
    }
}

// If this is a broken spanner, return the amount the left end is to
// be shifted horizontally so that the spanner starts after the
// initial clef and key on the staves. This is necessary for ties,
// slurs, crescendo and decrescendo signs, for example.
Real
Spanner::get_broken_left_end_align () const
{
  int i;
  Line_of_score *l;
  Score_column *sc = dynamic_cast<Score_column*> (spanned_drul_[LEFT]->column_l());

  // Relevant only if left span point is first column in line
  if(sc != NULL && sc->line_l ()->cols_.find_i (sc) == 0)
    {
      // We could possibly return the right edge of the whole Score_column here,
      // but we do a full search for the Break_align_item.
      for(i = 0; i < sc->elem_l_arr_.size (); i++)
	{
	  if(0 == strcmp (classname (sc->elem_l_arr_[i]), "Break_align_item"))
	    {
	      return sc->elem_l_arr_[i]->extent (X_AXIS) [RIGHT];
	    }
	}
    }

  return 0.0;
}
