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
#include "separation-item.hh"
#include "staff-spacing.hh"
#include "accidental-placement.hh"

void
Note_spacing::get_spacing (Grob *me, Item* right_col,
			   Real base_space, Real increment, Real *space, Real *fixed)
{

  Drul_array<SCM> props(me->get_grob_property ("left-items"),
			me->get_grob_property ("right-items"));
  Direction d = LEFT;
  Direction col_dir =  right_col->break_status_dir ();
  Drul_array<Interval> extents;

  Interval left_head_wid; 
  do
    {
      for (SCM  s = props[d]; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (gh_car(s)));
	  
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
	      SCM r = it->get_grob_property ("rest");
	      Grob * g = unsmob_grob (r);
	      if (!g)
		g =  Note_column::first_head (it);

	      if (g)
		left_head_wid = g->extent(it_col, X_AXIS);
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

      if (extents[d].empty_b ())
	extents[d] = Interval (0,0);
    }
  while (flip (&d) != LEFT);


  /*
    We look at the width of the note head, since smaller heads get less space
    eg. a quarter rest gets almost 0.5 ss less horizontal space than a note.

    What is sticking out of the note head (eg. a flag), doesn't get
    the full amount of space.
  */
  *fixed = left_head_wid.empty_b () ? increment : left_head_wid[RIGHT];
  *space = (base_space - increment) + *fixed +
    (extents[LEFT][RIGHT] - left_head_wid[RIGHT])/ 2;
    ;

  if (*space - *fixed < 2 * ((- extents[RIGHT][LEFT]) >? 0))
    {
      /*
    
      What's sticking out at the left of the right side has less
      influence. We only take it into account if there is not enough
      space.

      this sucks: this criterion is discontinuous; FIXME.
      */
      *space += 0.5 * (( -extents[RIGHT][LEFT]) >? 0);
    }

  stem_dir_correction (me, right_col, increment, space, fixed);
}

Item *
Note_spacing::left_column (Grob *me)
{
  if (!me->live())
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
  if (!me->live())
    return 0;
  
  SCM right = me->get_grob_property ("right-items");
  Item *mincol = 0;
  int min_rank = INT_MAX;
  bool prune = false;
  for (SCM s = right ; gh_pair_p (s) ; s = gh_cdr (s))
    {
      Item * ri = unsmob_item (gh_car (s));

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
      for (SCM s = right ; gh_pair_p (s) ; s =gh_cdr (s))
	{
	  if (unsmob_item (gh_car (s))->get_column () == mincol)
	    newright = gh_cons (gh_car (s), newright);
	}

      me->set_grob_property ("right-items", newright);
    }
  
  if (!mincol)
    {
      /*
      int r = Paper_column::get_rank (dynamic_cast<Item*>(me)->get_column ());
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

*/
void
Note_spacing::stem_dir_correction (Grob*me, Item * rcolumn,
				   Real increment,
				   Real * space, Real *fixed)  
{
  Drul_array<Direction> stem_dirs(CENTER,CENTER);
  Drul_array<Interval> stem_posns;
  Drul_array<Interval> head_posns;  
  Drul_array<SCM> props(me->get_grob_property ("left-items"),
			me->get_grob_property ("right-items"));

  Drul_array<Grob*> beams_drul(0,0);
  Real correction = 0.0;
  
  stem_dirs[LEFT] = stem_dirs[RIGHT] = CENTER;
  Interval intersect;
  Interval bar_xextent;
  Interval bar_yextent;  
  
  bool correct = true;
  Direction d = LEFT;
  bool acc_right = false;
  
  do
    {
      for (SCM  s = props[d]; gh_pair_p (s); s = gh_cdr (s))
	{
	  Item * it= dynamic_cast<Item*> (unsmob_grob (gh_car(s)));

	  if (d == RIGHT)
	    acc_right = acc_right || Note_column::accidentals (it);
	  
	  Grob *stem = Note_column::get_stem (it);

	  if (!stem)
	    {
	      if (d == RIGHT && Separation_item::has_interface (it))
		{
		  if (it->get_column () != rcolumn)
		    {
		      it = it->find_prebroken_piece (rcolumn->break_status_dir ());
		    }
		  
		  Grob *last = Staff_spacing::extremal_break_aligned_grob (it, LEFT, &bar_xextent);

		  if (last)
		    bar_yextent = Staff_spacing::bar_y_positions (last);

		  break;
		}

	      return ;
	    }
	  
	  if(Stem::invisible_b (stem))
	    {
	      return ;
	    }

	  beams_drul[d] = Stem::get_beam (stem);
	    
	  
	  Direction sd = Stem::get_direction (stem);
	  if (stem_dirs[d] && stem_dirs[d] != sd)
	    {
	      return ; 
	    }
	  stem_dirs[d] = sd;

	  /*
	    Correction doesn't seem appropriate  when there is a large flag
	    hanging from the note.
	   */
	  if (d == LEFT
	      && Stem::duration_log (stem) > 2  && !Stem::get_beam (stem))
	    {

	      return;
	    }
	  

	  
	  Interval hp  = Stem::head_positions (stem);
	  Real chord_start = hp[sd];	  
	  Real stem_end = Stem::stem_end_position (stem);
	  
	  stem_posns[d] = Interval(chord_start<?stem_end, chord_start>? stem_end);
	  head_posns[d].unite (hp);
	}
    }
  while (flip (&d) != LEFT);
  

  /*
    don't correct if accidentals are sticking out of the right side.

  */
  if (acc_right)
    return ;

  if (!bar_yextent.empty_b())
    {
      stem_dirs[RIGHT] = - stem_dirs[LEFT];
      stem_posns[RIGHT] = bar_yextent;
    }
  
  if (correct &&stem_dirs[LEFT] *stem_dirs[RIGHT] == -1)
    {
      if (beams_drul[LEFT] && beams_drul[LEFT] == beams_drul[RIGHT])
	{
	  /*
	    this is a knee: maximal correction.
	  */
	  
	  correction = increment* stem_dirs[LEFT];
	  *fixed += increment* stem_dirs[LEFT];
	}
      else
	{
	  intersect = stem_posns[LEFT];  
	  intersect.intersect(stem_posns[RIGHT]);
	  correct = correct && !intersect.empty_b ();

	  if (!correct)
	    return;
	  
	  correction = abs (intersect.length ());	  

      
	  /*
	    Ugh. 7 is hardcoded.
	  */
	  correction = (correction/7) <? 1.0;
	  correction *= stem_dirs[LEFT] ;
	  correction *= gh_scm2double (me->get_grob_property ("stem-spacing-correction"));

	  if (!bar_yextent.empty_b())
	    {
	      correction *= 0.5;
	    }
	}
    }
  else if (correct && stem_dirs[LEFT] *stem_dirs[RIGHT] == UP)
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
      if (!hp.empty_b())
	return ;

      Direction lowest =
	(head_posns[LEFT][DOWN] > head_posns[RIGHT][UP]) ? RIGHT : LEFT;

      Real delta = head_posns[-lowest][DOWN] - head_posns[lowest][UP] ;
      Real corr = gh_scm2double (me->get_grob_property ("stem-spacing-correction"));
      corr =  (delta <= 1) ? 0.0 : 0.25;
      
      correction=  -lowest * corr ;
    }

  if (!bar_xextent.empty_b())
    correction += - bar_xextent[LEFT];

  *space += correction;
}
 



ADD_INTERFACE (Note_spacing,"note-spacing-interface",
  "",
  "left-items right-items stem-spacing-correction");

