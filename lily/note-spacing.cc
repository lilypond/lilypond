/*
  note-spacing.cc -- implement Note_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "note-spacing.hh"

#include "directional-element-interface.hh"
#include "grob-array.hh"
#include "paper-column.hh"
#include "moment.hh"
#include "note-column.hh"
#include "warn.hh"
#include "stem.hh"
#include "separation-item.hh"
#include "staff-spacing.hh"
#include "accidental-placement.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"

/*
  TODO: detect hshifts due to collisions, and account for them in
  spacing?
*/

void
Note_spacing::get_spacing (Grob *me, Item *right_col,
			   Real base_space, Real increment, Real *space, Real *fixed)
{
  Drul_array<SCM> props (me->get_object ("left-items"),
			 me->get_object ("right-items"));
  Direction d = LEFT;
  Direction col_dir = right_col->break_status_dir ();
  Drul_array<Interval> extents;

  Interval left_head_wid;
  do
    {
      vector<Grob*> const &items (ly_scm2link_array (props [d]));
      for (vsize i = items.size (); i--;)
	{
	  Item *it = dynamic_cast<Item *> (items[i]);

	  if (d == RIGHT && it->break_status_dir () != col_dir)
	    it = it->find_prebroken_piece (col_dir);

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

	  if (d == LEFT
	      && Note_column::has_interface (it))
	    {
	      SCM r = it->get_object ("rest");
	      Grob *g = unsmob_grob (r);
	      if (!g)
		g = Note_column::first_head (it);

	      /*
		Ugh. If Stem is switched off, we don't know what the
		first note head will be.
	      */
	      if (g)
		{
		  if (g->common_refpoint (it_col, X_AXIS) != it_col)
		    programming_error ("Note_spacing::get_spacing (): Common refpoint incorrect");
		  else
		    left_head_wid = g->extent (it_col, X_AXIS);
		}
	    }

	  extents[d].unite (it->extent (it_col, X_AXIS));
	  if (d == RIGHT)
	    {
	      Grob *accs = Note_column::accidentals (it);
	      if (!accs)
		accs = Note_column::accidentals (it->get_parent (X_AXIS));

	      if (accs)
		{
		  Interval v
		    = Accidental_placement::get_relevant_accidental_extent (accs, it_col, me);

		  extents[d].unite (v);
		}

	      if (Grob *arpeggio = Note_column::arpeggio (it))
		extents[d].unite (arpeggio->extent (it_col, X_AXIS));
	    }
	}

      if (extents[d].is_empty ())
	extents[d] = Interval (0, 0);
    }
  while (flip (&d) != LEFT);

  /*
    We look at the width of the note head, since smaller heads get less space

    eg. a quarter rest gets almost 0.5 ss less horizontal space than a note.

    What is sticking out of the note head (eg. a flag), doesn't get
    the full amount of space.

    FIXED also includes the left part of the right object.
  */
  *fixed
    = (left_head_wid.is_empty () ? increment
       :        /*
		  Size of the head:
		*/
       (left_head_wid[RIGHT]+

	/*
	  What's sticking out of the head, eg. a flag:
	*/
	(extents[LEFT][RIGHT] - left_head_wid[RIGHT]) / 2))

    /*
      What is sticking out on the left side of the right note:
    */
    + (extents[RIGHT].is_empty ()
       ? 0.0
       : ((- extents[RIGHT][LEFT] / 2)

	  /*
	    Add that which sticks out a lot.
	  */
	  + max (0.0, -extents[RIGHT][LEFT] - (base_space - 0.5 * increment))));

  /*
    We don't do complicated stuff: (base_space - increment) is the
    normal amount of white, which also determines the amount of
    stretch. Upon (extreme) stretching, notes with accidentals should
    stretch as much as notes without accidentals.
  */
  *space = (base_space - increment) + *fixed;

  stem_dir_correction (me, right_col, increment, space, fixed);
}

Item *
Note_spacing::left_column (Grob *me)
{
  if (!me->is_live ())
    return 0;

  return dynamic_cast<Item *> (me)->get_column ();
}

/*
  Compute the column of the right-items.  This is a big function,
  since RIGHT-ITEMS may span more columns (eg. if a clef is inserted,
  this will add a new column to RIGHT-ITEMS. Here we look at the
  columns, and return the left-most. If there are multiple columns, we
  prune RIGHT-ITEMS.
*/
Item *
Note_spacing::right_column (Grob *me)
{
  if (!me->is_live ())
    return 0;

  Grob_array *a = unsmob_grob_array (me->get_object ("right-items"));
  Item *mincol = 0;
  int min_rank = INT_MAX;
  bool prune = false;
  for (vsize i = 0; a && i < a->size (); i++)
    {
      Item *ri = a->item (i);
      Item *col = ri->get_column ();

      int rank = Paper_column::get_rank (col);

      if (rank < min_rank)
	{
	  min_rank = rank;
	  if (mincol)
	    prune = true;

	  mincol = col;
	}
    }

  if (prune && a)
    {
      vector<Grob*> &right = a->array_reference ();
      for (vsize i = right.size (); i--;)
	{
	  if (dynamic_cast<Item *> (right[i])->get_column () != mincol)
	    right.erase (right.begin () + i);
	}
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
Note_spacing::stem_dir_correction (Grob *me, Item *rcolumn,
				   Real increment,
				   Real *space, Real *fixed)
{
  Drul_array<Direction> stem_dirs (CENTER, CENTER);
  Drul_array<Interval> stem_posns;
  Drul_array<Interval> head_posns;
  Drul_array<SCM> props (me->get_object ("left-items"),
			 me->get_object ("right-items"));

  Drul_array<Spanner *> beams_drul (0, 0);
  Drul_array<Grob *> stems_drul (0, 0);

  stem_dirs[LEFT] = stem_dirs[RIGHT] = CENTER;
  Interval intersect;
  Interval bar_xextent;
  Interval bar_yextent;

  bool correct_stem_dirs = true;
  Direction d = LEFT;
  bool acc_right = false;

  do
    {
      vector<Grob*> const &items (ly_scm2link_array (props [d]));
      for (vsize i = 0; i < items.size (); i++)
	{
	  Item *it = dynamic_cast<Item *> (items[i]);

	  if (d == RIGHT)
	    acc_right = acc_right || Note_column::accidentals (it);

	  Grob *stem = Note_column::get_stem (it);

	  if (!stem || !stem->is_live ())
	    {
	      if (d == RIGHT && Separation_item::has_interface (it))
		{
		  if (it->get_column () != rcolumn)
		    it = it->find_prebroken_piece (rcolumn->break_status_dir ());

		  Grob *last = Separation_item::extremal_break_aligned_grob (it, LEFT, &bar_xextent);

		  if (last)
		    bar_yextent = Staff_spacing::bar_y_positions (last);

		  break;
		}

	      return;
	    }

	  if (Stem::is_invisible (stem))
	    {
	      correct_stem_dirs = false;
	      continue;
	    }

	  stems_drul[d] = stem;
	  beams_drul[d] = Stem::get_beam (stem);

	  Direction stem_dir = get_grob_direction (stem);
	  if (stem_dirs[d] && stem_dirs[d] != stem_dir)
	    {
	      correct_stem_dirs = false;
	      continue;
	    }
	  stem_dirs[d] = stem_dir;

	  /*
	    Correction doesn't seem appropriate  when there is a large flag
	    hanging from the note.
	  */
	  if (d == LEFT
	      && Stem::duration_log (stem) > 2 && !Stem::get_beam (stem))
	    correct_stem_dirs = false;

	  Interval hp = Stem::head_positions (stem);
	  if (correct_stem_dirs
	      && !hp.is_empty ())
	    {
	      Real chord_start = hp[stem_dir];

	      /*
		can't look at stem-end-position, since that triggers
		beam slope computations.
	      */
	      Real stem_end = hp[stem_dir] +
		stem_dir * robust_scm2double (stem->get_property ("length"), 7);

	      stem_posns[d] = Interval (min (chord_start, stem_end),
					max (chord_start, stem_end));
	      head_posns[d].unite (hp);
	    }
	}
    }
  while (flip (&d) != LEFT);

  /*
    don't correct if accidentals are sticking out of the right side.
  */
  if (acc_right)
    return;

  Real correction = 0.0;

  if (!bar_yextent.is_empty ())
    {
      stem_dirs[RIGHT] = -stem_dirs[LEFT];
      stem_posns[RIGHT] = bar_yextent;
      stem_posns[RIGHT] *= 2;
    }

  if (correct_stem_dirs && stem_dirs[LEFT] * stem_dirs[RIGHT] == -1)
    {
      if (beams_drul[LEFT] && beams_drul[LEFT] == beams_drul[RIGHT])
	{

	  /*
	    this is a knee: maximal correction.
	  */
	  Real note_head_width = increment;
	  Grob *st = stems_drul[RIGHT];
	  Grob *head = st ? Stem::support_head (st) : 0;

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

	  correction = note_head_width * stem_dirs[LEFT];
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
	      correction = abs (intersect.length ());

	      /*
		Ugh. 7 is hardcoded.
	      */
	      correction = min (correction / 7, 1.0);
	      correction *= stem_dirs[LEFT];
	      correction
		*= robust_scm2double (me->get_property ("stem-spacing-correction"), 0);
	    }

	  if (!bar_yextent.is_empty ())
	    correction *= 0.5;
	}
    }
  else if (correct_stem_dirs && stem_dirs[LEFT] * stem_dirs[RIGHT] == UP)
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
      hp.intersect (head_posns[RIGHT]);
      if (!hp.is_empty ())
	return;

      Direction lowest
	= (head_posns[LEFT][DOWN] > head_posns[RIGHT][UP]) ? RIGHT : LEFT;

      Real delta = head_posns[-lowest][DOWN] - head_posns[lowest][UP];
      Real corr = robust_scm2double (me->get_property ("same-direction-correction"), 0);

      if (delta > 1)
	correction = -lowest * corr;
    }

  *space += correction;

  /* there used to be a correction for bar_xextent () here, but
     it's unclear what that was good for ?
  */
}

ADD_INTERFACE (Note_spacing,
	       "This object calculates spacing wishes for individual voices.",

	       
	       "knee-spacing-correction "
	       "left-items "
	       "right-items "
	       "same-direction-correction "
	       "stem-spacing-correction "

	       );

