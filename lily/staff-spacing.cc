/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2009  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "staff-spacing.hh"

#include <cstdio>
using namespace std;

#include "international.hh"
#include "paper-column.hh"
#include "separation-item.hh"
#include "warn.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "note-column.hh"
#include "stem.hh"
#include "spacing-interface.hh"
#include "accidental-placement.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"

/* A stem following a bar-line creates an optical illusion similar to the
   one mentioned in note-spacing.cc. We correct for it here.

   TODO: should we still correct if there are accidentals/arpeggios before
   the stem?
*/

Real
Staff_spacing::optical_correction (Grob *me, Grob *g, Interval bar_height)
{
  if (!g || !Note_column::has_interface (g))
    return 0;

  Grob *stem = Note_column::get_stem (g);
  Real ret = 0.0;

  if (!bar_height.is_empty () && stem)
    {
      Direction d = get_grob_direction (stem);
      if (Stem::is_normal_stem (stem) && d == DOWN)
	{

	  /*
	    can't look at stem-end-position, since that triggers
	    beam slope computations.
	  */
	  Real stem_start = Stem::head_positions (stem) [d];
	  Real stem_end = stem_start + 
	    d * robust_scm2double (stem->get_property ("length"), 7);
	  
	  Interval stem_posns (min (stem_start, stem_end),
			       max (stem_end, stem_start));

	  stem_posns.intersect (bar_height);

	  ret = min (abs (stem_posns.length () / 7.0), 1.0);
	  ret *= robust_scm2double (me->get_property ("stem-spacing-correction"), 1);
	}
    }
  return ret;
}

/*
  Y-positions that are covered by BAR_GROB, in the case that it is a
  barline.  */
Interval
Staff_spacing::bar_y_positions (Grob *bar_grob)
{
  Interval bar_size;
  bar_size.set_empty ();
  if (Bar_line::has_interface (bar_grob))
    {
      SCM glyph = bar_grob->get_property ("glyph-name");
      Grob *staff_sym = Staff_symbol_referencer::get_staff_symbol (bar_grob);

      string glyph_string = scm_is_string (glyph) ? ly_scm2string (glyph) : "";
      if (glyph_string.substr (0, 1) == "|"
	  || glyph_string.substr (0, 1) == ".")
	{
	  Grob *common = bar_grob->common_refpoint (staff_sym, Y_AXIS);
	  bar_size = bar_grob->extent (common, Y_AXIS);
	  bar_size *= 1.0 / Staff_symbol_referencer::staff_space (bar_grob);
	}
    }
  return bar_size;
}

Real
Staff_spacing::next_notes_correction (Grob *me,
				      Grob *last_grob)
{
  Interval bar_size = bar_y_positions (last_grob);
  Grob *orig = me->original () ? me->original () : me;
  vector<Item*> note_columns = Spacing_interface::right_note_columns (orig);

  Real max_optical = 0.0;

  for (vsize i = 0; i < note_columns.size (); i++)
    max_optical = max (max_optical, optical_correction (me, note_columns[i], bar_size));

  return max_optical;
}

/* We calculate three things here: the ideal distance, the minimum distance
   (which is the distance at which collisions will occur) and the "fixed"
   distance, which is the distance at which things start to look really bad.
   We arrange things so that the fixed distance will be attained when the
   line is compressed with a force of 1.0 */
Spring
Staff_spacing::get_spacing (Grob *me, Grob *right_col)
{
  Item *me_item = dynamic_cast<Item *> (me);
  Grob *left_col = me_item->get_column ();

  Interval last_ext;
  Direction break_dir = me_item->break_status_dir ();
  Grob *last_grob = Spacing_interface::extremal_break_aligned_grob (me, LEFT,
								    break_dir,
								    &last_ext);
  if (!last_grob)
    {
      /*
	TODO:

	Should  insert a adjustable space here? For excercises, you might want to
	use a staff without a clef in the beginning.
      */

      /*
	we used to have a warning here, but it generates a lot of
	spurious error messages.
      */
      return Spring ();
    }

  SCM alist = last_grob->get_property ("space-alist");
  if (!scm_list_p (alist))
    return Spring ();

  SCM space_def = scm_sloppy_assq (ly_symbol2scm ("first-note"), alist);
  if (me_item->break_status_dir () == CENTER)
    {
      SCM nndef = scm_sloppy_assq (ly_symbol2scm ("next-note"), alist);
      if (scm_is_pair (nndef))
	space_def = nndef;
    }

  if (!scm_is_pair (space_def))
    {
      programming_error ("unknown prefatory spacing");
      return Spring ();
    }

  space_def = scm_cdr (space_def);
  Real distance = scm_to_double (scm_cdr (space_def));
  SCM type = scm_car (space_def);

  Real fixed = last_ext[RIGHT];
  Real ideal = fixed + 1.0;

  if (type == ly_symbol2scm ("fixed-space"))
    {
      fixed += distance;
      ideal = fixed;
    }
  else if (type == ly_symbol2scm ("extra-space"))
    ideal = fixed + distance;
  else if (type == ly_symbol2scm ("semi-fixed-space"))
    {
      fixed += distance / 2;
      ideal = fixed + distance / 2;
    }
  else if (type == ly_symbol2scm ("minimum-space"))
    ideal = last_ext[LEFT] + max (last_ext.length (), distance);
  else if (type == ly_symbol2scm ("minimum-fixed-space"))
    {
      fixed = last_ext[LEFT] + max (last_ext.length (), distance);
      ideal = fixed;
    }


  Real optical_correction = next_notes_correction (me, last_grob);
  Real min_dist = Paper_column::minimum_distance (left_col, right_col);

  /* ensure that the "fixed" distance will leave a gap of at least 0.3 ss. */
  Real min_dist_correction = max (0.0, 0.3 + min_dist - fixed);
  Real correction = max (optical_correction, min_dist_correction);

  fixed += correction;
  ideal += correction;

  Spring ret (ideal, min_dist);
  ret.set_inverse_stretch_strength (max (0.0, ideal - fixed));
  return ret;
}

ADD_INTERFACE (Staff_spacing,
	       "This object calculates spacing details from a breakable"
	       " symbol (left) to another object.  For example, it takes care"
	       " of optical spacing from a bar line to a note.",

	       /* properties */
	       "stem-spacing-correction "
	       );
