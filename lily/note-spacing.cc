/*   
  note-spacing.cc -- implement Note_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "paper-column.hh"
#include "item.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "grob.hh"
#include "note-column.hh"
#include "warn.hh"
#include "stem.hh"
#include "separation-item.hh"
#include "staff-spacing.hh"
#include "accidental-placement.hh"
#include "paper-def.hh"


/*

TODO: detect hshifts due to collisions, and account for them in
spacing?

*/ 

void
Note_spacing::get_spacing (Grob *me, Item* right_col,
			   Real base_space, Real increment, Real *space, Real *fixed)
{

  Drul_array<SCM> props (me->get_property ("left-items"),
			me->get_property ("right-items"));
  Direction d = LEFT;
  Direction col_dir =  right_col->break_status_dir ();
  Drul_array<Interval> extents;

  Interval left_head_wid; 
  do
    {
      for (SCM  s = props[d]; ly_pair_p (s); s = ly_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (ly_car (s)));
	  
	  if (d == RIGHT && it->break_status_dir () != col_dir)
	    {
	      it = it -> find_prebroken_piece (col_dir);

	    }
	  /*
	    some kind of mismatch, eg. a note column, that is behind a
	    linebreak.
	   */
	  if (!it)
	    continue; 

	  Item *it_col = it->get_column ();
	  if (d == RIGHT && right_col != it_col)
	    continue;
	  
	  if (Separation_item::has_interface (it))
	    {
	      extents[d].unite (Separation_item::width (it));
	      continue;
	    }

	  if (d == LEFT)
	    {
	      SCM r = it->get_property ("rest");
	      Grob * g = unsmob_grob (r);
	      if (!g)
		g =  Note_column::first_head (it);

	      /*
		Ugh. If Stem is switched off, we don't know what the
		first note head will be.
	       */
	      if (g)
		left_head_wid = g->extent (it_col, X_AXIS);
	    }
	  
	  extents[d].unite (it->extent (it_col, X_AXIS));
	  if (d == RIGHT)
	    {
	      Grob * accs = Note_column::accidentals (it);
	      if (!accs)
		accs = Note_column::accidentals (it->get_parent (X_AXIS));
	      
	      if (accs)
		{
		  Interval v =
		    Accidental_placement::get_relevant_accidental_extent (accs, it_col, me);
		    
		  extents[d].unite (v);
		}
	    }
	}

      if (extents[d].is_empty ())
	extents[d] = Interval (0,0);
    }
  while (flip (&d) != LEFT);


  /*
    We look at the width of the note head, since smaller heads get less space
    eg. a quarter rest gets almost 0.5 ss less horizontal space than a note.

    What is sticking out of the note head (eg. a flag), doesn't get
    the full amount of space.

    FIXED also includes the left part of the right object.
  */
  *fixed =
    (left_head_wid.is_empty () ? increment :
     /*
       Size of the head:
      */
     (left_head_wid[RIGHT]+

      /*
       What's sticking out of the head, eg. a flag: 
      */
      (extents[LEFT][RIGHT] - left_head_wid[RIGHT])/2))

    /*
      What is sticking out of the right note:
     */
    + (extents[RIGHT].is_empty () ?  0.0 : - extents[RIGHT][LEFT] / 2);

  /*
    We don't do complicated stuff: (base_space - increment) is the
    normal amount of white, which also determines the amount of
    stretch. Upon (extreme) stretching, notes with accidentals should
    stretch as much as notes without accidentals.
   */
  *space = (base_space - increment) + *fixed ;

  if (!extents[RIGHT].is_empty ()
      && (Item::is_breakable (right_col)
	  || right_col->original_))
    {
      /*
	This is for the situation

	rest | 3/4 (eol)

	Since we only take half of the right-object space above, the
	barline will bump into the notes preceding it, if the right
	thing is big. We add the rest of the extents here:
       */
      
      *space += -extents[RIGHT][LEFT] / 2;
      *fixed += -extents[RIGHT][LEFT] / 2;
    }
  
  stem_dir_correction (me, right_col, increment, space, fixed);
}

Item *
Note_spacing::left_column (Grob *me)
{
  if (!me->live ())
    return 0;
  
  return dynamic_cast<Item*> (me)->get_column ();
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
  if (!me->live ())
    return 0;
  
  SCM right = me->get_property ("right-items");
  Item *mincol = 0;
  int min_rank = INT_MAX;
  bool prune = false;
  for (SCM s = right ; ly_pair_p (s) ; s = ly_cdr (s))
    {
      Item * ri = unsmob_item (ly_car (s));

      Item * col = ri->get_column ();
      int rank = Paper_column::get_rank (col);

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
      for (SCM s = right ; ly_pair_p (s) ; s =ly_cdr (s))
	{
	  if (unsmob_item (ly_car (s))->get_column () == mincol)
	    newright = scm_cons (ly_car (s), newright);
	}

      me->set_property ("right-items", newright);
    }
  
  if (!mincol)
    {
      /*
      int r = Paper_column::get_rank (dynamic_cast<Item*>(me)->get_column ());
      programming_error (_f ("Spacing wish column %d has no right item.", r));
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

*/
void
Note_spacing::stem_dir_correction (Grob*me, Item * rcolumn,
				   Real increment,
				   Real * space, Real *fixed)  
{
  Drul_array<Direction> stem_dirs (CENTER,CENTER);
  Drul_array<Interval> stem_posns;
  Drul_array<Interval> head_posns;  
  Drul_array<SCM> props (me->get_property ("left-items"),
			me->get_property ("right-items"));

  Drul_array<Grob*> beams_drul (0,0);
  Drul_array<Grob*> stems_drul (0,0);
  
  stem_dirs[LEFT] = stem_dirs[RIGHT] = CENTER;
  Interval intersect;
  Interval bar_xextent;
  Interval bar_yextent;  
  
  bool correct_stem_dirs = true;
  Direction d = LEFT;
  bool acc_right = false;
  
  do
    {
      for (SCM  s = props[d]; ly_pair_p (s); s = ly_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (ly_car (s)));

	  if (d == RIGHT)
	    acc_right = acc_right || Note_column::accidentals (it);
	  
	  Grob *stem = Note_column::get_stem (it);

	  if (!stem || !stem->live ())
	    {
	      if (d == RIGHT && Separation_item::has_interface (it))
		{
		  if (it->get_column () != rcolumn)
		    {
		      it = it->find_prebroken_piece (rcolumn->break_status_dir ());
		    }
		  
		  Grob *last = Separation_item::extremal_break_aligned_grob (it, LEFT, &bar_xextent);

		  if (last)
		    bar_yextent = Staff_spacing::bar_y_positions (last);

		  break;
		}

	      return ;
	    }
	  
	  if (Stem::is_invisible (stem))
	    {
	      correct_stem_dirs = false;
	      continue;
	    }

	  stems_drul[d] = stem;
	  beams_drul[d] = Stem::get_beam (stem);
	    
	  
	  Direction sd = Stem::get_direction (stem);
	  if (stem_dirs[d] && stem_dirs[d] != sd)
	    {
	      correct_stem_dirs = false;
	      continue;
	    }
	  stem_dirs[d] = sd;

	  /*
	    Correction doesn't seem appropriate  when there is a large flag
	    hanging from the note.
	   */
	  if (d == LEFT
	      && Stem::duration_log (stem) > 2  && !Stem::get_beam (stem))
	    {
	      correct_stem_dirs = false;
	    }
	  
	  Interval hp  = Stem::head_positions (stem);
	  Real chord_start = hp[sd];	  
	  Real stem_end = Stem::stem_end_position (stem);
	  
	  stem_posns[d] = Interval (chord_start<?stem_end, chord_start>? stem_end);
	  head_posns[d].unite (hp);
	}
    }
  while (flip (&d) != LEFT);
  

  /*
    don't correct if accidentals are sticking out of the right side.
  */
  if (acc_right)
    return ;

  Real correction = 0.0;

  if (!bar_yextent.is_empty ())
    {
      stem_dirs[RIGHT] = - stem_dirs[LEFT];
      stem_posns[RIGHT] = bar_yextent;
    }
  
  if (correct_stem_dirs && stem_dirs[LEFT] *stem_dirs[RIGHT] == -1)
    {
      if (beams_drul[LEFT] && beams_drul[LEFT] == beams_drul[RIGHT])
	{
	  
	  /*
	    this is a knee: maximal correction.
	  */
	  Real note_head_width = increment;
	  Grob * st = stems_drul[RIGHT];
	  Grob * head = st ? Stem::support_head (st)  : 0;

	  Interval head_extent;
	  if (head)
	    {
	      head_extent = head->extent (rcolumn, X_AXIS);

	      if (!head_extent.is_empty ())
		note_head_width = head_extent[RIGHT];

	      if (st)
		{
		  Real thick = Stem::thickness (st);

		  note_head_width -= thick;
		}
	    }

	  correction = note_head_width* stem_dirs[LEFT];
	  correction *= robust_scm2double (me->get_property ("knee-spacing-correction"), 0);
	  *fixed += correction;
	}
      else
	{
	  intersect = stem_posns[LEFT];  
	  intersect.intersect (stem_posns[RIGHT]);
	  correct_stem_dirs = correct_stem_dirs && !intersect.is_empty ();

	  if (correct_stem_dirs)
	    {
	      correction =abs (intersect.length ());

      
	      /*
		Ugh. 7 is hardcoded.
	      */
	      correction = (correction/7) <? 1.0;
	      correction *= stem_dirs[LEFT] ;
	      correction *=
		robust_scm2double (me->get_property ("stem-spacing-correction"), 0);
	    }
	  
	  if (!bar_yextent.is_empty ())
	    {
	      correction *= 0.5;
	    }
	}
    }
  else if (correct_stem_dirs && stem_dirs[LEFT] *stem_dirs[RIGHT] == UP)
    {
      /*
	Correct for the following situation:

	 X      X
	|      | 
	|      |
 	|   X  |
	|  |   |
	========

           ^ move the center one to the left.
	

	this effect seems to be much more subtle than the
	stem-direction stuff (why?), and also does not scale with the
	difference in stem length.
	
       */

      
      Interval hp = head_posns[LEFT];
      hp.intersect  (head_posns[RIGHT]);
      if (!hp.is_empty ())
	return ;

      Direction lowest =
	(head_posns[LEFT][DOWN] > head_posns[RIGHT][UP]) ? RIGHT : LEFT;

      Real delta = head_posns[-lowest][DOWN] - head_posns[lowest][UP] ;
      Real corr = robust_scm2double (me->get_property ("stem-spacing-correction"), 0);
      corr =  (delta <= 1) ? 0.0 : 0.25;
      
      correction=  -lowest * corr ;
    }

  *space += correction;

  /* there used to be a correction for bar_xextent () here, but
     it's unclear what that was good for ?
  */

}
 



ADD_INTERFACE (Note_spacing,"note-spacing-interface",
  "This object calculates spacing wishes for individual voices.",
  "left-items right-items stem-spacing-correction knee-spacing-correction");

