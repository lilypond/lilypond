/*
  spanner.cc -- implement Spanner

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>
#include <libc-extension.hh>


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
Spanner::do_break_processing ()
{
  //break_into_pieces
  Item * left = spanned_drul_[LEFT];
  Item * right = spanned_drul_[RIGHT];

  if (!left || !right)
    return;
  
  /*
    Check if our parent in X-direction spans equally wide
    or wider than we do.
   */
  for (int a = X_AXIS; a < NO_AXES; a ++)
    {
      if (Spanner* parent = dynamic_cast<Spanner*> (parent_l ((Axis)a)))
	{
	  if (!parent->spanned_rank_iv ().contains_b (this->spanned_rank_iv ()))
	    {
	      programming_error (to_str ("Spanner `%s' is not fully contained in parent spanner `%s'.",
					 classname (this),
					 classname (parent)));
	    }
	}
    }
  
  if (line_l () || broken_b ())
    return;

  if  (left == right)
    {
      /*
	If we have a spanner spanning one column, we must break it
	anyway because it might provide a parent for another item.  */
      Direction d = LEFT;
      do
	{
	  Item* bound = left->find_prebroken_piece (d);
	  if (!bound)
	    programming_error ("no broken bound");
	  else if (bound->line_l ())
	    {
	      Spanner * span_p = dynamic_cast<Spanner*>( clone ());
	      span_p->set_bound (LEFT, bound);
	      span_p->set_bound (RIGHT, bound);

	      assert (span_p->line_l ()); 
	      span_p->line_l ()->typeset_element (span_p);
	      broken_into_l_arr_.push (span_p);
	    }
	}
      while ((flip(&d))!= LEFT);
    }
  else
    {
      Link_array<Item> break_points = pscore_l_->line_l_->broken_col_range (left,right);

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
	      if (!bounds[d]->line_l())
		bounds[d] = bounds[d]->find_prebroken_piece(- d);
	    }
	  while ((flip(&d))!= LEFT);

  	  if (!bounds[LEFT] ||  ! bounds[RIGHT])
	    {
	      programming_error ("bounds of this piece aren't breakable. ");
	      continue; 
	    }

	  Spanner *span_p = dynamic_cast<Spanner*>(clone ());
	  span_p->set_bound(LEFT,bounds[LEFT]);
	  span_p->set_bound(RIGHT,bounds[RIGHT]);


	  assert (bounds[LEFT]->line_l () ==
		  bounds[RIGHT]->line_l ());

	  bounds[LEFT]->line_l ()->typeset_element (span_p);
	  broken_into_l_arr_.push (span_p);
	}
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
	set_bound(i,spanned_drul_[i]->find_prebroken_piece((Direction) -i));
    } 
  while (flip(&i) != LEFT);
}       

Interval_t<int>
Spanner::spanned_rank_iv ()
{
  Interval_t<int> iv (0, 0);

  if (spanned_drul_[LEFT])
    {
      iv[LEFT] = Paper_column::rank_i (spanned_drul_[LEFT]->column_l ());
    }
  if (spanned_drul_[RIGHT])
    {
      iv[RIGHT] = Paper_column::rank_i (spanned_drul_[RIGHT]->column_l ());
    }
  return iv;
}

Item*
Spanner::get_bound (Direction d) const
{
  return spanned_drul_ [d];
}

void
Spanner::set_bound(Direction d, Score_element*s)
{
  Item * i = dynamic_cast<Item*> (s);
  if (!i)
    {
      programming_error ("Must have Item for spanner bound.");
      return;
    }
  
  spanned_drul_[d] =i;

  /**
     We check for Line_of_score to prevent the column -> line_of_score
     -> column -> line_of_score -> etc situation */
  if (d== LEFT && !dynamic_cast<Line_of_score*> (this))
    {
      set_parent (i, X_AXIS);
    }
}


Spanner::Spanner (SCM s)
  : Score_element (s)
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
  return  p1->line_l ()->rank_i_ - p2->line_l ()->rank_i_;
}

bool
Spanner::broken_b() const
{
  return broken_into_l_arr_.size();
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

void
Spanner::do_derived_mark ()
{
  Direction d = LEFT;
  do
    if (spanned_drul_[d])
      scm_gc_mark (spanned_drul_[d]->self_scm_);
  while (flip (&d) != LEFT);

  for (int i= broken_into_l_arr_.size () ; i--;)
    scm_gc_mark (broken_into_l_arr_[i]->self_scm_);
}

void
add_bound_item (Spanner* sp, Item*it)
{
  if (!sp->get_bound (LEFT))
    sp->set_bound (LEFT, it);
  else
    sp->set_bound (RIGHT, it);
}
