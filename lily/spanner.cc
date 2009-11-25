/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "libc-extension.hh"
#include "moment.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "pointer-group-interface.hh"
#include "stencil.hh"
#include "system.hh"
#include "warn.hh"

Grob *
Spanner::clone () const
{
  return new Spanner (*this);
}

void
Spanner::do_break_processing ()
{
  //break_into_pieces
  Item *left = spanned_drul_[LEFT];
  Item *right = spanned_drul_[RIGHT];

  if (!left || !right)
    return;


  if (get_system () || is_broken ())
    return;

  if (left == right)
    {
      /*
	If we have a spanner spanning one column, we must break it
	anyway because it might provide a parent for another item.  */
      Direction d = LEFT;
      do
	{
	  Item *bound = left->find_prebroken_piece (d);
	  if (!bound)
	    programming_error ("no broken bound");
	  else if (bound->get_system ())
	    {
	      Spanner *span = dynamic_cast<Spanner *> (clone ());
	      span->set_bound (LEFT, bound);
	      span->set_bound (RIGHT, bound);

	      assert (span->get_system ());
	      span->get_system ()->typeset_grob (span);
	      broken_intos_.push_back (span);
	    }
	}
      while ((flip (&d)) != LEFT);
    }
  else
    {
      System *root  = get_root_system (this);
      vector<Item*> break_points = root->broken_col_range (left, right);

      break_points.insert (break_points.begin () + 0, left);
      break_points.push_back (right);

      Slice parent_rank_slice;
      parent_rank_slice.set_full ();
      
      /*
	Check if our parent in X-direction spans equally wide
	or wider than we do.
      */
      for (int a = X_AXIS; a < NO_AXES; a++)
	{
	  if (Spanner *parent = dynamic_cast<Spanner *> (get_parent ((Axis)a)))
	    parent_rank_slice.intersect (parent->spanned_rank_interval ());
	}
  
      for (vsize i = 1; i < break_points.size (); i++)
	{
	  Drul_array<Item *> bounds;
	  bounds[LEFT] = break_points[i - 1];
	  bounds[RIGHT] = break_points[i];
	  Direction d = LEFT;
	  do
	    {
	      if (!bounds[d]->get_system ())
		bounds[d] = bounds[d]->find_prebroken_piece (- d);
	    }
	  while ((flip (&d)) != LEFT);

	  if (!bounds[LEFT] || ! bounds[RIGHT])
	    {
	      programming_error ("bounds of this piece aren't breakable. ");
	      continue;
	    }

	  bool ok = parent_rank_slice.contains (bounds[LEFT]->get_column ()->get_rank ());
	  ok = ok && parent_rank_slice.contains (bounds[RIGHT]->get_column ()->get_rank ());
	  
	  if (!ok)
	    {
	      programming_error (to_string ("Spanner `%s' is not fully contained in parent spanner. Ignoring orphaned part",
					    name ().c_str ()));
	      continue;
	    }
	    
	  
	  Spanner *span = dynamic_cast<Spanner *> (clone ());
	  span->set_bound (LEFT, bounds[LEFT]);
	  span->set_bound (RIGHT, bounds[RIGHT]);

	  if (!bounds[LEFT]->get_system ()
	      || !bounds[RIGHT]->get_system ()
	      || bounds[LEFT]->get_system () != bounds[RIGHT]->get_system ())
	    {
	      programming_error ("bounds of spanner are invalid");
	      span->suicide ();
	    }
	  else
	    {
	      bounds[LEFT]->get_system ()->typeset_grob (span);
	      broken_intos_.push_back (span);
	    }
	}
    }
  vector_sort (broken_intos_, Spanner::less);
  for (vsize i = broken_intos_.size (); i--;)
    broken_intos_[i]->break_index_ = i;
}

vsize
Spanner::get_break_index () const
{
  return break_index_;
}

void
Spanner::set_my_columns ()
{
  Direction i = (Direction) LEFT;
  do
    {
      if (!spanned_drul_[i]->get_system ())
	set_bound (i, spanned_drul_[i]->find_prebroken_piece ((Direction) -i));
    }
  while (flip (&i) != LEFT);
}

Interval_t<int>
Spanner::spanned_rank_interval () const
{
  Interval_t<int> iv (0, 0);

  if (spanned_drul_[LEFT] && spanned_drul_[LEFT]->get_column ())
    iv[LEFT] = spanned_drul_[LEFT]->get_column ()->get_rank ();
  if (spanned_drul_[RIGHT] && spanned_drul_[RIGHT]->get_column ())
    iv[RIGHT] = spanned_drul_[RIGHT]->get_column ()->get_rank ();
  return iv;
}

Interval_t<Moment>
Spanner::spanned_time () const
{
  return spanned_time_interval (spanned_drul_[LEFT],
				spanned_drul_[RIGHT]);
}


Item *
Spanner::get_bound (Direction d) const
{
  return spanned_drul_[d];
}

/*
  Set the items that this spanner spans. If D == LEFT, we also set the
  X-axis parent of THIS to S.
*/
void
Spanner::set_bound (Direction d, Grob *s)
{
  Item *i = dynamic_cast<Item *> (s);
  if (!i)
    {
      programming_error ("must have Item for spanner bound of " + name());
      return;
    }

  spanned_drul_[d] = i;

  /**
     We check for System to prevent the column -> line_of_score
     -> column -> line_of_score -> etc situation */
  if (d == LEFT && !dynamic_cast<System *> (this))
    set_parent (i, X_AXIS);

  /*
    Signal that this column needs to be kept alive. They need to be
    kept alive to have meaningful position and linebreaking.

    [maybe we should try keeping all columns alive?, and perhaps
    inherit position from their (non-)musical brother]
  */
  if (dynamic_cast<Paper_column *> (i))
    Pointer_group_interface::add_grob (i, ly_symbol2scm ("bounded-by-me"), this);
}

Spanner::Spanner (SCM s)
  : Grob (s)
{
  break_index_ = 0;
  spanned_drul_.set (0, 0);
}

Spanner::Spanner (Spanner const &s)
  : Grob (s)
{
  spanned_drul_.set (0, 0);
}

Real
Spanner::spanner_length () const
{
  Real l = spanned_drul_[LEFT]->relative_coordinate (0, X_AXIS);
  Real r = spanned_drul_[RIGHT]->relative_coordinate (0, X_AXIS);

  if (r < l)
    programming_error ("spanner with negative length");

  return r - l;
}

System *
Spanner::get_system () const
{
  if (!spanned_drul_[LEFT] || !spanned_drul_[RIGHT])
    return 0;
  if (spanned_drul_[LEFT]->get_system () != spanned_drul_[RIGHT]->get_system ())
    return 0;
  return spanned_drul_[LEFT]->get_system ();
}

Grob *
Spanner::find_broken_piece (System *l) const
{
  vsize idx = binary_search (broken_intos_, (Spanner *) l, Spanner::less);
  if (idx != VPOS)
    return broken_intos_ [idx];
  return 0;
}

Spanner *
Spanner::broken_neighbor (Direction d) const
{
  if (!original_)
    return 0;

  vsize k = broken_spanner_index (this);
  Spanner *orig = dynamic_cast<Spanner*> (original_);
  int j = int (k) + d;
  if (j < 0 || vsize (j) >= orig->broken_intos_.size ())
    return 0;

  return orig->broken_intos_[j];
}

int
Spanner::compare (Spanner *const &p1, Spanner *const &p2)
{
  return p1->get_system ()->get_rank () - p2->get_system ()->get_rank ();
}

bool
Spanner::less (Spanner *const &a, Spanner *const &b)
{
  return a->get_system ()->get_rank () < b->get_system ()->get_rank ();
}

bool
Spanner::is_broken () const
{
  return broken_intos_.size ();
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
  Paper_column *sc = dynamic_cast<Paper_column *> (spanned_drul_[LEFT]->get_column ());

  // Relevant only if left span point is first column in line
  if (sc != NULL
      && sc->break_status_dir () == RIGHT)
    {
      /*
	We used to do a full search for the Break_align_item.
	But that doesn't make a difference, since the Paper_column
	is likely to contain only a Break_align_item.
      */
      return sc->extent (sc, X_AXIS)[RIGHT];
    }

  return 0.0;
}

void
Spanner::derived_mark () const
{
  Direction d = LEFT;
  do
    if (spanned_drul_[d])
      scm_gc_mark (spanned_drul_[d]->self_scm ());
  while (flip (&d) != LEFT)
    ;

  for (vsize i = broken_intos_.size (); i--;)
    scm_gc_mark (broken_intos_[i]->self_scm ());
}

/*
  Set left or right bound to IT.

  Warning: caller should ensure that subsequent calls put in ITems
  that are left-to-right ordered.
*/
void
add_bound_item (Spanner *sp, Grob *it)
{
  if (!sp->get_bound (LEFT))
    sp->set_bound (LEFT, it);
  else
    sp->set_bound (RIGHT, it);
}

MAKE_SCHEME_CALLBACK (Spanner, set_spacing_rods, 1);
SCM
Spanner::set_spacing_rods (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  SCM num_length = me->get_property ("minimum-length");
  if (scm_is_number (num_length))
    {
      Rod r;
      Spanner *sp = dynamic_cast<Spanner *> (me);
      System *root = get_root_system (me);
      Drul_array<Item*> bounds (sp->get_bound (LEFT),
				sp->get_bound (RIGHT));
      if (!bounds[LEFT] || !bounds[RIGHT])
	return SCM_UNSPECIFIED;
      
      vector<Item*> cols (root->broken_col_range (bounds[LEFT]->get_column (),
						  bounds[RIGHT]->get_column ()));

      if (cols.size ())
	{
	  Rod r ;
	  r.item_drul_[LEFT] = sp->get_bound (LEFT);
	  r.item_drul_[RIGHT] = cols[0]->find_prebroken_piece (LEFT);
	  r.distance_ = robust_scm2double (num_length, 0);
	  r.add_to_cols ();
	  
	  r.item_drul_[LEFT] = cols.back ()->find_prebroken_piece (RIGHT);
	  r.item_drul_[RIGHT] = sp->get_bound (RIGHT);
	  r.add_to_cols ();
	}
	   
      r.distance_ = robust_scm2double (num_length, 0);
      r.item_drul_[LEFT] = sp->get_bound (LEFT);
      r.item_drul_[RIGHT] = sp->get_bound (RIGHT);
      r.add_to_cols ();
    }
  
  return SCM_UNSPECIFIED;
}

/*
  Return I such that SP == SP->ORIGINAL ()->BROKEN_INTOS_[I].
*/
int
broken_spanner_index (Spanner const *sp)
{
  Spanner *parent = dynamic_cast<Spanner *> (sp->original ());
  /* ugh: casting */
  return find (parent->broken_intos_, (Spanner*) sp) - parent->broken_intos_.begin ();
}

Spanner *
unsmob_spanner (SCM s)
{
  return dynamic_cast<Spanner *> (unsmob_grob (s));
}

MAKE_SCHEME_CALLBACK (Spanner, bounds_width, 1);
SCM
Spanner::bounds_width (SCM grob)
{
  Spanner *me = unsmob_spanner (grob);

  Grob *common = me->get_bound (LEFT)->common_refpoint (me->get_bound (RIGHT), X_AXIS);

  Interval w (me->get_bound (LEFT)->relative_coordinate (common, X_AXIS),
	      me->get_bound (RIGHT)->relative_coordinate (common, X_AXIS));
	      
  w -= me->relative_coordinate (common, X_AXIS);

  return ly_interval2scm (w);
}

MAKE_SCHEME_CALLBACK (Spanner, kill_zero_spanned_time, 1);
SCM
Spanner::kill_zero_spanned_time (SCM grob)
{
  Spanner *me = unsmob_spanner (grob);
  Interval_t<Moment> moments = me->spanned_time ();
  /*
    Remove the line or hairpin at the start of the line.  For
    piano voice indicators, it makes no sense to have them at
    the start of the line.

    I'm not sure what the official rules for glissandi are, but
    usually the 2nd note of the glissando is "exact", so when playing
    from the start of the line, there is no need to glide.

    From a typographical p.o.v. this makes sense, since the amount of
    space left of a note at the start of a line is very small.

    --hwn.

  */
  if (moments.length () == Moment (0, 0))
    me->suicide ();

  return SCM_UNSPECIFIED;
}

ADD_INTERFACE (Spanner,
	       "Some objects are horizontally spanned between objects.  For"
	       " example, slurs, beams, ties, etc.  These grobs form a subtype"
	       " called @code{Spanner}.  All spanners have two span points"
	       " (these must be @code{Item} objects), one on the left and one"
	       " on the right.  The left bound is also the X@tie{}reference"
	       " point of the spanner.",

	       /* properties */
	       "minimum-length "
	       "to-barline "
	       );

