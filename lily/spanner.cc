/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "debug.hh"
#include "spanner.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "molecule.hh"
#include "paper-outputter.hh"
#include "paper-column.hh"
#include "line-of-score.hh"
#include "break-align-item.hh"

void
Spanner::do_break_processing()
{
  //break_into_pieces
  
  if (line_l () || broken_b ())
    return;
  
  Item * left = spanned_drul_[LEFT];
  Item * right = spanned_drul_[RIGHT];

  if  (left == right)
    {
      warning (_ ("Left spanpoint is right spanpoint"));
      return;
    }
  
  Link_array<Item> break_points = pscore_l_->broken_col_range (left,right);

  break_points.insert (left,0);
  break_points.push (right);

  for (int i=1; i < break_points.size(); i++) 
    {
      Drul_array<Item*> bounds;
      bounds[LEFT] = break_points[i-1];
      bounds[RIGHT] = break_points[i];
      Direction d = LEFT;
      do
	{
	  Item *&pc_l = bounds[d] ;
	  if (!pc_l->line_l())
	    pc_l =  pc_l->find_broken_piece(- d);
	  
	  assert (pc_l);
	}
      while ((flip(&d))!= LEFT);

      Spanner *span_p = dynamic_cast<Spanner*>(clone ());
      span_p->set_bounds(LEFT,bounds[LEFT]);
      span_p->set_bounds(RIGHT,bounds[RIGHT]);
      
      pscore_l_->typeset_element (span_p);
      broken_into_l_arr_.push (span_p);
    }

  broken_into_l_arr_.sort (Spanner::compare);
}

void
Spanner::set_my_columns()
{
  Direction i = (Direction) LEFT;
  do 
    {
      if (!spanned_drul_[i]->line_l())
	set_bounds(i,spanned_drul_[i]->find_broken_piece((Direction) -i));
    } 
  while (flip(&i) != LEFT);
}       

void
Spanner::set_bounds(Direction d, Item*i)
{
  spanned_drul_[d] =i;
  if (i)
    {
      i->used_b_ = true;
    }

  /**
     Prevent the column -> line_of_score -> column -> line_of_score -> etc situation
  */
  if (d== LEFT && !dynamic_cast<Line_of_score*> (this))
    {
      set_parent (i, X_AXIS);
    }
  
  if (spanned_drul_[Direction(-d)] == spanned_drul_[d]
       && i)
    warning (_f ("Spanner `%s' has equal left and right spanpoints", classname (this)));
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


Real
Spanner::spanner_length() const
{  
  Real l = spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS);
  Real r = spanned_drul_[RIGHT]->relative_coordinate (0, X_AXIS);

  if (r< l)
    programming_error ("spanner with negative length");

  return r-l;
}

Line_of_score *
Spanner::line_l() const
{
  if (!spanned_drul_[LEFT] || !spanned_drul_[RIGHT])
    return 0;
  if (spanned_drul_[LEFT]->line_l () != spanned_drul_[RIGHT]->line_l ())
    return 0;
  return spanned_drul_[LEFT]->line_l();
}


Score_element*
Spanner::find_broken_piece (Line_of_score*l) const
{
  int idx = binsearch_link_array (broken_into_l_arr_,  (Spanner*)l, Spanner::compare);
  
  if (idx < 0)
    return 0;
  else
    return broken_into_l_arr_ [idx];
}


int
Spanner::compare (Spanner * const &p1, Spanner * const &p2)
{
  return p1->line_l ()->rank_i_ - p2->line_l ()->rank_i_;
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
  If this is a broken spanner, return the amount the left end is to be
  shifted horizontally so that the spanner starts after the initial
  clef and key on the staves. This is necessary for ties, slurs,
  crescendo and decrescendo signs, for example.
*/
Real
Spanner::get_broken_left_end_align () const
{
  Paper_column *sc = dynamic_cast<Paper_column*> (spanned_drul_[LEFT]->column_l());

  // Relevant only if left span point is first column in line
  if(sc != NULL &&
     sc->break_status_dir () == RIGHT)
    {
      /*
	
	We used to do a full search for the Break_align_item.
	But that doesn't make a difference, since the Paper_column
	is likely to contain only a Break_align_item.
      */
      return sc->extent (X_AXIS)[RIGHT];
    }

  return 0.0;
}
