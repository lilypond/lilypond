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
#include "spacing-interface.hh"
#include "staff-spacing.hh"
#include "accidental-placement.hh"
#include "output-def.hh"
#include "pointer-group-interface.hh"

/*
  TODO: detect hshifts due to collisions, and account for them in
  spacing?
*/

Spring
Note_spacing::get_spacing (Grob *me, Item *right_col,
			   Real base_space, Real increment)
{
  vector<Item*> note_columns = Spacing_interface::left_note_columns (me);
  Real left_head_end = 0;

  for (vsize i = 0; i < note_columns.size (); i++)
    {
       SCM r = note_columns[i]->get_object ("rest");
       Grob *g = unsmob_grob (r);
       Grob *col = note_columns[i]->get_column ();

       if (!g)
	 g = Note_column::first_head (note_columns[i]);

       /*
	 Ugh. If Stem is switched off, we don't know what the
	 first note head will be.
       */
       if (g)
	 {
	   if (g->common_refpoint (col, X_AXIS) != col)
	     programming_error ("Note_spacing::get_spacing (): Common refpoint incorrect");
	   else
	     left_head_end = g->extent (col, X_AXIS)[RIGHT];
	 }
    }

  /*
    We look at the width of the note head, since smaller heads get less space

    eg. a quarter rest gets almost 0.5 ss less horizontal space than a note.

    What is sticking out of the note head (eg. a flag), doesn't get
    the full amount of space.
  */
  Real min_dist = Spacing_interface::minimum_distance (me, right_col);
  Real min_desired_space = max (left_head_end + (min_dist - left_head_end) / 2,
				min_dist - (base_space - increment) / 2);
  Real ideal = base_space - increment + min_desired_space;

  stem_dir_correction (me, right_col, increment, &ideal, &min_desired_space);

  Spring ret (ideal, min_dist);
  ret.set_inverse_compress_strength (ideal - max (min_dist, min_desired_space));
  return ret;
}


/**
   Correct for optical illusions. See [Wanske] p. 138. The combination
   up-stem + down-stem should get extra space, the combination
   down-stem + up-stem less.

   TODO: have to check whether the stems are in the same staff.
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

	      note_head_width -= Stem::thickness (st);
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

