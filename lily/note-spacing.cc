/*   
  note-spacing.cc -- implement Note_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "paper-column.hh"
#include "item.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "grob.hh"
#include "note-column.hh"
#include "warn.hh"
#include "stem.hh"

bool
Note_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("note-spacing-interface"));
}

Real
Note_spacing::get_spacing (Grob *me)
{
  Drul_array<SCM> props(me->get_grob_property ("left-items"),
			me->get_grob_property ("right-items"));
  Direction d = LEFT;
  Drul_array<Interval> extents;
  do
    {
      for (SCM  s = props[d]; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (gh_car(s)));
	  extents[d].unite (it->extent (it->column_l (), X_AXIS));

	  if (d == RIGHT)
	    {
	      Grob * accs = Note_column::accidentals (it);
	      if (!accs)
		accs = Note_column::accidentals (it->get_parent (X_AXIS));
	      
	      if (accs)
		extents[d].unite (accs->extent (it->column_l (), X_AXIS));
	    }
	}

      if (extents[d].empty_b ())
	extents[d] = Interval (0,0);
    }
  while (flip (&d) != LEFT);

  /*
    
    What's sticking out at the left of the right side has less
    influence.

  */
  Real dx= extents[LEFT][RIGHT] - 0.5 * extents[RIGHT][LEFT];
  return dx;
}

Item *
Note_spacing::left_column (Grob *me)
{
  if (me->immutable_property_alist_ == SCM_EOL)
    return 0;
  
  return dynamic_cast<Item*> (me)->column_l ();
}

/*
  Compute the column of the right-items.  This is a big function,
since RIGHT-ITEMS may span more columns (eg. if a clef if inserted,
this will add a new columns to RIGHT-ITEMS. Here we look at the
columns, and return the left-most. If there are multiple columns, we
prune RIGHT-ITEMS.
   
 */
Item *
Note_spacing::right_column (Grob*me)
{
  /*
    ugh. should have generic is_live() method?
   */
  if (me->immutable_property_alist_ == SCM_EOL)
    return 0;
  
  SCM right = me->get_grob_property ("right-items");
  Item *mincol = 0;
  int min_rank = INT_MAX;
  bool prune = false;
  for (SCM s = right ; gh_pair_p (s) ; s = gh_cdr (s))
    {
      Item * ri = unsmob_item (gh_car (s));

      Item * col = ri->column_l ();
      int rank = Paper_column::rank_i (col);

      if (rank < min_rank)
	{
	  min_rank = rank;
	  if (mincol)
	    prune = true;

	  mincol = col;
	}
    }
  
  if (prune)
    {
      // I'm a lazy bum. We could do this in-place.
      SCM newright  = SCM_EOL;
      for (SCM s = right ; gh_pair_p (s) ; s =gh_cdr (s))
	{
	  if (unsmob_item (gh_car (s))->column_l () == mincol)
	    newright = gh_cons (gh_car (s), newright);
	}

      me->set_grob_property ("right-items", newright);
    }
  
  if (!mincol)
    {
      /*
      int r = Paper_column::rank_i (dynamic_cast<Item*>(me)->column_l ());
      programming_error (_f("Spacing wish column %d has no right item.", r));
      */

      return 0;
    }

  return mincol;
}

/**
   Correct for optical illusions. See [Wanske] p. 138. The combination
   up-stem + down-stem should get extra space, the combination
   down-stem + up-stem less.

   TODO: have to check wether the stems are in the same staff.

   TODO: also correct for bar lines in RIGHT-ITEMS.  Should check if
   the barline is the leftmost object of the break alignment.

*/
Real
Note_spacing::stem_dir_correction (Grob*me) 
{
  Drul_array<Direction> stem_dirs(CENTER,CENTER);
  Drul_array<Interval> posns;
  Drul_array<SCM> props(me->get_grob_property ("left-items"),
			me->get_grob_property ("right-items"));

  stem_dirs[LEFT] = stem_dirs[RIGHT] = CENTER;
  Interval intersect;
  bool correct = true;
  Direction d = LEFT;
  do
    {
      for (SCM  s = props[d]; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (gh_car(s)));

	  Grob *stem = Note_column::stem_l (it);

	  if (!stem || Stem::invisible_b (stem))
	    {
	      correct = false;
	      goto exit_loop ;
	    }

	  Direction sd = Stem::get_direction (stem);
	  if (stem_dirs[d] && stem_dirs[d] != sd)
	    {
	      correct = false;
	      goto exit_loop;
	    }
	  stem_dirs[d] = sd;

	  Real chord_start = Stem::head_positions (stem)[sd];
	  Real stem_end = Stem::stem_end_position (stem);
	  
	  posns[d] = Interval(chord_start<?stem_end, chord_start>? stem_end);
	}
    }
  while (flip (&d) != LEFT);
  
  intersect = posns[LEFT];  
  intersect.intersect(posns[RIGHT]);
  correct = correct && !intersect.empty_b ();
  correct = correct && (stem_dirs[LEFT] *stem_dirs[RIGHT] == -1);
  
 exit_loop:
  if(!correct)
    return 0.0;

  /*
    Ugh. 7 is hardcoded.
   */
  Real correction = abs (intersect.length ());
  correction = (correction/7) <? 1.0;
  correction *= stem_dirs[LEFT] ;
  correction *= gh_scm2double (me->get_grob_property ("stem-spacing-correction"));

  return correction;
}
 
