/*
  staff-spacing.cc -- implement Staff_spacing

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "staff-spacing.hh"

#include <cstdio>
using namespace std;

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

/*
  Insert some more space for the next note, in case it has a stem in
  the wrong direction
*/
void
Staff_spacing::next_note_correction (Grob *me,
				     Grob *g,
				     Interval bar_size,
				     Real *space,
				     Real *fix)
{
  (void)space;
  
  if (!g || !Note_column::has_interface (g))
    return ;

  Item *col = dynamic_cast<Item *> (g)->get_column ();
  Real max_corr = max (0., (- g->extent (col, X_AXIS)[LEFT]));

  /*
    Duh. If this gets out of hand, we should invent something more generic.
  */
  if (Grob *a = Note_column::accidentals (g))
    {
      Interval v;
      if (Accidental_placement::has_interface (a))
	v = Accidental_placement::get_relevant_accidental_extent (a, col, me);
      else
	v = a->extent (col, X_AXIS);

      max_corr = max (max_corr, (- v[LEFT]));
    }
  if (Grob *a = unsmob_grob (g->get_object ("arpeggio")))
    max_corr = max (max_corr, - a->extent (col, X_AXIS)[LEFT]);

  /*
    Let's decrease the space a little if the problem is not located
    after a barline.
  */
  if (bar_size.is_empty ())
    max_corr *= 0.75;

  if (!bar_size.is_empty ())
    if (Grob *stem = Note_column::get_stem (g))
      {
	Direction d = get_grob_direction (stem);
	if (d == DOWN)
	  {
	    Real stem_start = Stem::head_positions (stem) [DOWN];
	    Real stem_end = Stem::stem_end_position (stem);
	    Interval stem_posns (min (stem_start, stem_end),
				 max (stem_end, stem_start));

	    stem_posns.intersect (bar_size);

	    Real corr = min (abs (stem_posns.length () / 7.0), 1.0);
	    corr *= robust_scm2double (me->get_property ("stem-spacing-correction"), 1);

	    if (d != DOWN)
	      corr = 0.0;
	    max_corr = max (max_corr, corr);
	  }
      }

  *fix += max_corr;
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
	  Interval bar_size = bar_grob->extent (common, Y_AXIS);
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
void
Staff_spacing::next_notes_correction (Grob *me, Grob *last_grob,
				      Real *compound_space, Real *compound_fixed
				      )
{
  Interval bar_size = bar_y_positions (last_grob);

  extract_grob_set (me, "right-items", right_items);

  *compound_fixed = 0.0;
  *compound_space = 0.0;
  int wish_count = 0;
  
  for (vsize i = right_items.size (); i--;)
    {
      Grob *g = right_items[i];

      Real space = 0.0;
      Real fixed = 0.0;

      next_note_correction (me, g, bar_size, &space, &fixed);

      *compound_space += space;
      *compound_fixed += fixed; 
      wish_count ++;
      
      extract_grob_set (g, "elements", elts);
      for (vsize j = elts.size (); j--;)
	{
	  Real space = 0.0;
	  Real fixed = 0.0;
	  next_note_correction (me, elts[j], bar_size, &space, &fixed);
	  *compound_fixed += fixed;
	  *compound_space += space;
	  wish_count ++;
	}
    }

  if (wish_count)
    {
      *compound_space /= wish_count;
      *compound_fixed /= wish_count;
    }
}

void
Staff_spacing::get_spacing_params (Grob *me, Real *space, Real *fixed)
{
  *space = 1.0;
  *fixed = 1.0;

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
      return;
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
      return;
    }

  *fixed = last_ext[RIGHT];
  *space = *fixed + 1.0;

  SCM alist = last_grob->get_property ("space-alist");
  if (!scm_list_p (alist))
    return;

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
      return;
    }

  space_def = scm_cdr (space_def);
  Real distance = scm_to_double (scm_cdr (space_def));
  SCM type = scm_car (space_def);

  *fixed = last_ext[RIGHT];
  if (type == ly_symbol2scm ("fixed-space"))
    {
      *fixed += distance;
      *space = *fixed;
    }
  else if (type == ly_symbol2scm ("extra-space"))
    *space = *fixed + distance;
  else if (type == ly_symbol2scm ("semi-fixed-space"))
    {
      *fixed += distance / 2;
      *space = *fixed + distance / 2;
    }
  else if (type == ly_symbol2scm ("minimum-space"))
    *space = last_ext[LEFT] + max (last_ext.length (), distance);
  else if (type == ly_symbol2scm ("minimum-fixed-space"))
    {
      *space = last_ext[LEFT] + max (last_ext.length (), distance);
      *fixed = *space;
    }

  Real correction_fixed, correction_space;
  next_notes_correction (me, last_grob, &correction_space, &correction_fixed );
  *space += correction_space;
  *fixed += correction_fixed;
}

ADD_INTERFACE (Staff_spacing, "staff-spacing-interface",
	       "This object calculates spacing details from a "
	       " breakable symbol (left) to another object. For example, it takes care "
	       " of  optical spacing from  a bar lines to a note.",

	       /* properties */
	       "stem-spacing-correction "
	       "left-items "
	       "right-items "
	       );
