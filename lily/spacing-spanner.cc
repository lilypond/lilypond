/*
  spacing-spanner.cc -- implement Spacing_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1999--2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include <math.h>

#include <cstdio>
using namespace std;

#include "spacing-spanner.hh"
#include "paper-column.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "system.hh"
#include "moment.hh"
#include "note-spacing.hh"
#include "main.hh"
#include "warn.hh"
#include "pointer-group-interface.hh"
#include "spaceable-grob.hh"
#include "staff-spacing.hh"
#include "spacing-interface.hh"


/*
  TODO:

  use callback instead?

*/
Rational
Spacing_spanner::effective_shortest_duration (Grob *me,
					      Link_array<Grob> const &all)
{
  SCM preset_shortest = me->get_property ("common-shortest-duration");
  Rational global_shortest;
  if (unsmob_moment (preset_shortest))
    global_shortest = unsmob_moment (preset_shortest)->main_part_;
  else
    {
      global_shortest = Spacing_spanner::find_shortest (me, all);
      if (be_verbose_global)
	message (_f ("Global shortest duration is %s", global_shortest.to_string ()) + "\n");
    }

  return global_shortest;
}


MAKE_SCHEME_CALLBACK (Spacing_spanner, set_springs, 1);
SCM
Spacing_spanner::set_springs (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  /*
    can't use get_system() ? --hwn.
  */
  Link_array<Grob> all (get_root_system (me)->columns ());

  set_explicit_neighbor_columns (all);

  Spacing_options options;
  options.init_from_grob (me);
  options.global_shortest_ = effective_shortest_duration (me, all);

  prune_loose_columns (me, &all, &options);
  set_implicit_neighbor_columns (all);
  generate_springs (me, all, &options);

  return SCM_UNSPECIFIED;
}

/*
  We want the shortest note that is also "common" in the piece, so we
  find the shortest in each measure, and take the most frequently
  found duration.

  This probably gives weird effects with modern music, where every
  note has a different duration, but hey, don't write that kind of
  stuff, then.
*/
Rational
Spacing_spanner::find_shortest (Grob *me, Link_array<Grob> const &cols)
{
  /*
    ascending in duration
  */
  Array<Rational> durations;
  Array<int> counts;

  Rational shortest_in_measure;
  shortest_in_measure.set_infinite (1);

  for (int i = 0; i < cols.size (); i++)
    {
      if (Paper_column::is_musical (cols[i]))
	{
	  Moment *when = unsmob_moment (cols[i]->get_property ("when"));

	  /*
	    ignore grace notes for shortest notes.
	  */
	  if (when && when->grace_part_)
	    continue;

	  SCM st = cols[i]->get_property ("shortest-starter-duration");
	  Moment this_shortest = *unsmob_moment (st);
	  assert (this_shortest.to_bool ());
	  shortest_in_measure = min (shortest_in_measure, this_shortest.main_part_);
	}
      else if (!shortest_in_measure.is_infinity ()
	       && Item::is_breakable (cols[i]))
	{
	  int j = 0;
	  for (; j < durations.size (); j++)
	    {
	      if (durations[j] > shortest_in_measure)
		{
		  counts.insert (1, j);
		  durations.insert (shortest_in_measure, j);
		  break;
		}
	      else if (durations[j] == shortest_in_measure)
		{
		  counts[j]++;
		  break;
		}
	    }

	  if (durations.size () == j)
	    {
	      durations.push (shortest_in_measure);
	      counts.push (1);
	    }

	  shortest_in_measure.set_infinite (1);
	}
    }

  int max_idx = -1;
  int max_count = 0;
  for (int i = durations.size (); i--;)
    {
      if (counts[i] >= max_count)
	{
	  max_idx = i;
	  max_count = counts[i];
	}

      // printf ("duration %d/%d, count %d\n",
      // durations[i].num (), durations[i].den (), counts[i]);
    }

  SCM bsd = me->get_property ("base-shortest-duration");
  Rational d = Rational (1, 8);
  if (Moment *m = unsmob_moment (bsd))
    d = m->main_part_;

  if (max_idx >= 0)
    d = min (d, durations[max_idx]);

  return d;
}

void
Spacing_spanner::generate_pair_spacing (Grob *me,
					Paper_column *left_col, Paper_column *right_col,
					Paper_column *after_right_col,
					Spacing_options const *options)
{
  if (Paper_column::is_musical (left_col))
    {
      bool skip_unbroken_right = false;

      if (!Paper_column::is_musical (right_col)
	  && options->float_nonmusical_columns_
	  && after_right_col
	  && Paper_column::is_musical (after_right_col))
	skip_unbroken_right = true;

      if (skip_unbroken_right)
	{
	  /*
	    TODO: should generate rods to prevent collisions.
	  */
	  musical_column_spacing (me, left_col, after_right_col, options);
	  right_col->set_object ("between-cols", scm_cons (left_col->self_scm (),
							   after_right_col->self_scm ()));
	}
      else
	musical_column_spacing (me, left_col, right_col, options);

      if (Item *rb = right_col->find_prebroken_piece (LEFT))
	musical_column_spacing (me, left_col, rb, options);
    }
  else
    {
      /*
	The case that the right part is broken as well is rather
	rare, but it is possible, eg. with a single empty measure,
	or if one staff finishes a tad earlier than the rest.
      */
      Item *lb = left_col->find_prebroken_piece (RIGHT);
      Item *rb = right_col->find_prebroken_piece (LEFT);

      if (left_col && right_col)
	breakable_column_spacing (me, left_col, right_col, options);

      if (lb && right_col)
	breakable_column_spacing (me, lb, right_col, options);

      if (left_col && rb)
	breakable_column_spacing (me, left_col, rb, options);

      if (lb && rb)
	breakable_column_spacing (me, lb, rb, options);
    }
}

void
Spacing_spanner::generate_springs (Grob *me,
				   Link_array<Grob> const &cols,
				   Spacing_options const *options)
{
  Paper_column *next = 0;
  Paper_column *next_next = 0;
  for (int i = cols.size (); i--;)
    {
      Paper_column *col = dynamic_cast<Paper_column *> (cols[i]);
      if (next)
	generate_pair_spacing (me, col, next, next_next, options);

      next_next = next;
      next = col;
    }
}

/*
  Generate the space between two musical columns LEFT_COL and RIGHT_COL, given
  spacing parameters INCR and SHORTEST.
*/
void
Spacing_spanner::musical_column_spacing (Grob *me,
					 Item *left_col,
					 Item *right_col,
					 Spacing_options const *options)
{
  bool expand_only = false;
  Real base_note_space = note_spacing (me, left_col, right_col, options, &expand_only);

  Real max_fixed = 0;
  Real max_space = 0;
  Real compound_note_space = 0.0;
  Real compound_fixed_note_space = 0.0;

  if (options->stretch_uniformly_)
    compound_note_space = base_note_space;
  else
    {
      int wish_count = 0;

      extract_grob_set (left_col, "right-neighbors", neighbors);

      /*
	We adjust the space following a note only if the next note
	happens after the current note (this is set in the grob
	property SPACING-SEQUENCE.
      */
      for (int i = 0; i < neighbors.size (); i++)
	{
	  Grob *wish = neighbors[i];

	  Item *wish_rcol = Note_spacing::right_column (wish);
	  if (Note_spacing::left_column (wish) != left_col
	      || (wish_rcol != right_col && wish_rcol != right_col->original ()))
	    continue;

	  /*
	    This is probably a waste of time in the case of polyphonic
	    music.  */
	  if (Note_spacing::has_interface (wish))
	    {
	      Real space = 0.0;
	      Real fixed = 0.0;

	      Note_spacing::get_spacing (wish, right_col, base_note_space, options->increment_, &space, &fixed);


	      max_space = max (max_space, space);
	      max_fixed = max (max_fixed, fixed);
	      
	      compound_note_space += space;
	      compound_fixed_note_space += fixed;
	      wish_count++;
	    }
	}

      if (Paper_column::when_mom (right_col).grace_part_
	  && !Paper_column::when_mom (left_col).grace_part_)
	{
	  /*
	    Ugh. 0.8 is arbitrary.
	  */
	  compound_note_space *= 0.8;
	}

      if (compound_note_space < 0 || wish_count == 0)
	{
	  compound_note_space = base_note_space;
	  compound_fixed_note_space = options->increment_;
	}
      else if (to_boolean (me->get_property ("average-spacing-wishes")))
	{
	  compound_note_space /= wish_count;
	  compound_fixed_note_space /= wish_count;
	}
      else
	{
	  compound_fixed_note_space = max_fixed;
	  compound_note_space = max_space;
	}

      /*
	Whatever we do, the fixed space is smaller than the real
	space.

	TODO: this criterion is discontinuous in the derivative.
	Maybe it should be continuous?
      */
      compound_fixed_note_space = min (compound_fixed_note_space,
				       compound_note_space);
    }

  Real inverse_strength = 1.0;
  Real distance = 1.0;

  /*
    TODO: make sure that the space doesn't exceed the right margin.
  */
  if (options->packed_)
    {
      /*
	In packed mode, pack notes as tight as possible.  This makes
	sense mostly in combination with raggedright mode: the notes
	are then printed at minimum distance.  This is mostly useful
	for ancient notation, but may also be useful for some flavours
	of contemporary music.  If not in raggedright mode, lily will
	pack as much bars of music as possible into a line, but the
	line will then be stretched to fill the whole linewidth.
      */
      inverse_strength = 1.0;
      distance = compound_fixed_note_space;
    }
  else
    {
      inverse_strength = (compound_note_space - compound_fixed_note_space);
      distance = compound_note_space;
    }

  Spaceable_grob::add_spring (left_col, right_col, distance, inverse_strength);
}

/*
  Read hints from L and generate springs.
*/
void
Spacing_spanner::breakable_column_spacing (Grob *me, Item *l, Item *r,
					   Spacing_options const *options)
{
  Real compound_fixed = 0.0;
  Real compound_space = 0.0;
  Real max_fixed = 0.0;
  Real max_space = 0.0;
  
  int wish_count = 0;

  Moment dt = Paper_column::when_mom (r) - Paper_column::when_mom (l);

  if (dt == Moment (0, 0))
    {
      extract_grob_set (l, "spacing-wishes", wishes);

      for (int i = 0; i < wishes.size (); i++)
	{
	  Item *spacing_grob = dynamic_cast<Item *> (wishes[i]);

	  if (!spacing_grob || !Staff_spacing::has_interface (spacing_grob))
	    continue;

	  Real space;
	  Real fixed_space;

	  /*
	    column for the left one settings should be ok due automatic
	    pointer munging.

	  */
	  assert (spacing_grob->get_column () == l);

	  Staff_spacing::get_spacing_params (spacing_grob,
					     &space, &fixed_space);

	  if (Paper_column::when_mom (r).grace_part_)
	    {
	      /*
		Correct for grace notes.

		Ugh. The 0.8 is arbitrary.
	      */
	      space *= 0.8;
	    }

	  max_space = max (max_space, space);
	  max_fixed = max (max_fixed, fixed_space);
	  
	  compound_space += space;
	  compound_fixed += fixed_space;
	  wish_count++;
	}
    }

  if (compound_space <= 0.0 || !wish_count)
    {
      standard_breakable_column_spacing (me, l, r, &compound_fixed, &compound_space,
					 options);
      wish_count = 1;
    }
  else
    {
      if (to_boolean (me->get_property ("average-spacing-wishes")))
	{
	  compound_space /= wish_count;
	  compound_fixed /= wish_count;
	}
      else
	{
	  compound_fixed = max_fixed;
	  compound_space = max_space;
	}
      
    }

  if (options->stretch_uniformly_ && l->break_status_dir () != RIGHT)
    compound_fixed = 0.0;

  assert (!isinf (compound_space));
  compound_space = max (compound_space, compound_fixed);

  /*
    There used to be code that changed spacing depending on
    raggedright setting.  Ugh.

    Do it more cleanly, or rename the property.

  */
  Real inverse_strength = (compound_space - compound_fixed);
  Real distance = compound_space;
  Spaceable_grob::add_spring (l, r, distance, inverse_strength);
}

ADD_INTERFACE (Spacing_spanner, "spacing-spanner-interface",
	       "The space taken by a note is dependent on its duration. Doubling a\n"
	       "duration adds spacing-increment to the space. The most common shortest\n"
	       "note gets @code{shortest-duration-space}. Notes that are even shorter are\n"
	       "spaced proportonial to their duration.\n"
	       "\n"
	       "Typically, the increment is the width of a black note head.  In a\n"
	       "piece with lots of 8th notes, and some 16th notes, the eighth note\n"
	       "gets 2 note heads width (i.e. the space following a note is 1 note\n"
	       "head width) A 16th note is followed by 0.5 note head width. The\n"
	       "quarter note is followed by  3 NHW, the half by 4 NHW, etc.\n",

	       "average-spacing-wishes "
	       "grace-space-factor "
	       "spacing-increment "
	       "base-shortest-duration "
	       "strict-note-spacing "
	       "shortest-duration-space "
	       "common-shortest-duration "
	       "uniform-stretching "
	       "packed-spacing "
	       );

ADD_INTERFACE (Spacing_interface, "spacing-interface",
	       "Something to do with line breaking and spacing. "
	       "Kill this one after determining line breaks.",
	       "");

