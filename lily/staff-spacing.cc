/*
  staff-spacing.cc -- implement Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "accidental-placement.hh"
#include "pointer-group-interface.hh"
#include "directional-element-interface.hh"

Real
Staff_spacing::left_stickout (Grob *me, Grob *g)
{
  if (!g || !Note_column::has_interface (g))
    return 0;

  Item *col = dynamic_cast<Item *> (g)->get_column ();
  Real ret = max (0., (- g->extent (col, X_AXIS)[LEFT]));

  /*
    Duh. If this gets out of hand, we should invent something more generic.
  */
  Grob *accs = Note_column::accidentals (g);
  if (accs)
    {
      Interval v;
      if (Accidental_placement::has_interface (accs))
	v = Accidental_placement::get_relevant_accidental_extent (accs, col, me);
      else
	v = accs->extent (col, X_AXIS);
      
      ret = max (ret, (- v[LEFT]));
    }
  Grob *arpeggio = unsmob_grob (g->get_object ("arpeggio"));
  if (arpeggio)
    ret = max (ret, - arpeggio->extent (col, X_AXIS)[LEFT]);

  return ret;
}

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

/*
  Do corrections for the following notes.

  This is slightly convoluted, since the staffspacing grob gets
  pointers to the separation-items, not the note-columns or
  note-spacings.
*/

Real
Staff_spacing::next_notes_correction (Grob *me,
				      Grob *last_grob,
				      Real current_fixed,
				      Real right_stickout)
{
  Interval bar_size = bar_y_positions (last_grob);
  Grob *orig = me->original () ? me->original () : me;

  extract_grob_set (orig, "right-items", right_items);

  Real max_left_stickout = 0.0;
  Real max_optical = 0.0;

  for (vsize i = right_items.size (); i--;)
    {
      Grob *g = right_items[i];
      if (Note_column::has_interface (right_items[i]))
	{
	  max_left_stickout = max (max_left_stickout, left_stickout (me, g));
	  max_optical = max (max_optical, optical_correction (me, g, bar_size));
	}
      else
	{
	  extract_grob_set (g, "elements", elts);
	  for (vsize j = elts.size (); j--;)
	    {
	      max_left_stickout = max (max_left_stickout, left_stickout (me, elts[j]));
	      max_optical = max (max_optical, optical_correction (me, g, bar_size));
	    }
	}
    }

  /* we put a minimum distance of 0.3 between the right edge of the left grob and
     the left edge of the right grob */
  max_left_stickout = max (0.0, max_left_stickout + right_stickout + 0.3 - current_fixed);
  return max (max_left_stickout, max_optical);
}

/* This routine does not impose any minimum distances between columns; it only
   affects springs. As such, the FIXED variable does not refer to a minimum
   distance between columns, but instead to a minimum desired distance between
   columns -- this ends up affecting the stiffness of a spring. In fact, FIXED
   will be the distance between columns if there is a compression force of 1.0
   applied to the line. */
Spring
Staff_spacing::get_spacing_params (Grob *me)
{
  Grob *separation_item = 0;
  Item *me_item = dynamic_cast<Item *> (me);

  extract_grob_set (me, "left-items", items);
  for (vsize i = items.size (); i--;)
    {
      Grob *cand = items[i];
      if (cand && Separation_item::has_interface (cand))
	separation_item = cand;
    }

  //  printf ("doing col %d\n" , Paper_column::get_rank (left_col));
  if (!separation_item)
    {
      programming_error ("no sep item");
      return Spring ();
    }

  Interval last_ext;
  Grob *last_grob = Separation_item::extremal_break_aligned_grob (separation_item, RIGHT,
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

  Real correction = next_notes_correction (me, last_grob, fixed, last_ext[RIGHT]);
  fixed += correction;
  ideal += correction;

  Spring ret;
  ret.min_distance_ = 0.0;
  ret.distance_ = ideal;
  ret.inverse_stretch_strength_ = ret.inverse_compress_strength_ = ideal - fixed;
  return ret;
}

ADD_INTERFACE (Staff_spacing,
	       "This object calculates spacing details from a "
	       " breakable symbol (left) to another object. For example, it takes care "
	       " of  optical spacing from  a bar lines to a note.",

	       /* properties */
	       "stem-spacing-correction "
	       );
