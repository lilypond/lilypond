/*   
     staff-spacing.cc --  implement Staff_spacing

     source file of the GNU LilyPond music typesetter

     (c) 2001--2004  Han-Wen Nienhuys <hanwen@cs.uu.nl>

*/

#include "staff-spacing.hh"

#include <cstdio>

#include "paper-column.hh" 
#include "separation-item.hh"
#include "warn.hh"
#include "bar-line.hh"
#include "staff-symbol-referencer.hh"
#include "note-column.hh"
#include "stem.hh"
#include "accidental-placement.hh"

/*
  Insert some more space for the next note, in case it has a stem in
  the wrong direction

 */
Real
Staff_spacing::next_note_correction (Grob * me,
				     Grob * g,
				     Interval bar_size)
{
  if (!g || !Note_column::has_interface (g))
    return 0.0;

  Item *col =dynamic_cast<Item*> (g)->get_column ();
  Real max_corr = 0. >? (- g->extent (col, X_AXIS)[LEFT]);

  /*
    Duh. If this gets out of hand, we should invent something more generic.
   */
  if (Grob * a = Note_column::accidentals (g))
    {
      Interval v;
      if (Accidental_placement::has_interface (a))
	{
	  v = Accidental_placement::get_relevant_accidental_extent (a, col, me);
	}
      else
	v = a->extent (col, X_AXIS);
      
      max_corr = max_corr >? (- v[LEFT]);
    }
  if (Grob* a = unsmob_grob (g->get_property ("arpeggio")))
    {
      max_corr = max_corr >? (- a->extent (col, X_AXIS)[LEFT]);
    }
  
  /*
    Let's decrease the space a little if the problem is not located
    after a barline.
  */
  if (bar_size.is_empty ())
    max_corr *= 0.75;
  
  if (!bar_size.is_empty ())
    if (Grob *stem = Note_column::get_stem (g))
      {
	Direction d = Stem::get_direction (stem);
	if (d == DOWN)
	  {
	    Real stem_start = Stem::head_positions (stem) [DOWN];
	    Real stem_end = Stem::stem_end_position (stem); 
	    Interval stem_posns (stem_start <? stem_end,
				 stem_end >? stem_start);

	    stem_posns.intersect (bar_size);

	    Real corr = abs (stem_posns.length ()/7.) <? 1.0;
	    corr *=
	      robust_scm2double (me->get_property ("stem-spacing-correction"), 1);

	    if (d != DOWN)
	      corr = 0.0;
	    max_corr = max_corr >? corr;
	  }
      }
  return max_corr;
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
      SCM glyph = bar_grob->get_property ("glyph");
      
      String glyph_string = scm_is_string (glyph) ? ly_scm2string (glyph) : "";
      if (glyph_string.left_string (1) == "|" || glyph_string.left_string (1) == ".")
	{
	  SCM sz = Bar_line::get_staff_bar_size (bar_grob->self_scm ());
	  bar_size = Interval (-1,1);
	  bar_size *= robust_scm2double (sz, 1)
	    / Staff_symbol_referencer::staff_space (bar_grob);
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
Staff_spacing::next_notes_correction (Grob *me, Grob * last_grob)
{
  Interval bar_size = bar_y_positions (last_grob);
  Real max_corr =0.0;

  for (SCM s = me->get_property ("right-items");
       scm_is_pair (s);  s = scm_cdr (s))
    {
      Grob * g = unsmob_grob (scm_car (s));

      max_corr = max_corr >?  next_note_correction (me, g,  bar_size);
      for (SCM t = g->get_property ("elements");
	   scm_is_pair (t); t  = scm_cdr (t))
	max_corr = max_corr >? next_note_correction (me, unsmob_grob (scm_car (t)), bar_size);
      
    }
  
  return max_corr;
}

void
Staff_spacing::get_spacing_params (Grob *me, Real * space, Real * fixed)
{
  *space = 1.0;
  *fixed = 1.0;

  Grob * separation_item=0;
  Item * me_item  = dynamic_cast<Item*> (me);
    
  for (SCM s = me->get_property ("left-items");
       scm_is_pair (s); s = scm_cdr (s))
    {
      Grob * cand = unsmob_grob (scm_car (s));
      if (cand && Separation_item::has_interface (cand))
	separation_item = cand ;
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
	we used to have a warning here, but itgenerates a lot of
	spurious error messages.
      */
      return ;
    }

  *fixed = last_ext[RIGHT];
  *space = *fixed + 1.0;
  
  SCM alist = last_grob->get_property ("space-alist");
  if (!scm_list_p (alist))
    return ;
  
  SCM space_def = scm_sloppy_assq (ly_symbol2scm ("first-note"), alist);
  if (me_item->break_status_dir () == CENTER)
    {
      SCM nndef = scm_sloppy_assq (ly_symbol2scm ("next-note"), alist);
      if (scm_is_pair (nndef))
	space_def = nndef;
    }
  
  
  if (!scm_is_pair (space_def))
    {
      programming_error ("Unknown prefatory spacing. "); 
      return; 
    }

  space_def = scm_cdr (space_def);
  Real distance = scm_to_double (scm_cdr (space_def));
  SCM type = scm_car (space_def) ;

  *fixed = last_ext[RIGHT];
  if (type == ly_symbol2scm ("fixed-space"))
    {
     *fixed += distance;
     *space = *fixed;
    }
  else if (type == ly_symbol2scm ("extra-space")) 
    {
      *space = *fixed + distance;
    }
  else if (type == ly_symbol2scm ("semi-fixed-space"))
    {
      *fixed += distance / 2; 
      *space =  *fixed + distance/2;
    }
  else if (type == ly_symbol2scm ("minimum-space"))
    {
      *space = last_ext[LEFT] + (last_ext.length () >? distance);
    }
  else if (type == ly_symbol2scm ("minimum-fixed-space"))
    {
      *space = last_ext[LEFT] + (last_ext.length () >? distance);
      *fixed = *space;
    }
  
  *space += next_notes_correction (me, last_grob);
}


ADD_INTERFACE (Staff_spacing,"staff-spacing-interface",
	       "This object calculates spacing details from a "
	       " breakable symbol (left) to another object. For example, it takes care "
	       " of  optical spacing from  a bar lines to a note.",
  "stem-spacing-correction left-items right-items");
