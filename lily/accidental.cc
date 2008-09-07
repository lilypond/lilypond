/*
  accidental.cc -- implement Accidental_interface

  source file of the GNU LilyPond music typesetter

  (c) 2001--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "accidental-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "stencil.hh"

Stencil
parenthesize (Grob *me, Stencil m)
{
  Font_metric * font
    = Font_interface::get_default_font (me); 
  Stencil open
    = font->find_by_name ("accidentals.leftparen");
  Stencil close
    = font->find_by_name ("accidentals.rightparen");

  m.add_at_edge (X_AXIS, LEFT, Stencil (open), 0);
  m.add_at_edge (X_AXIS, RIGHT, Stencil (close), 0);

  return m;
}

/* If this gets called before line breaking, we will return a non-trivial
   extent even if we belong to a tie and won't actually get printed. */
static SCM
get_extent (Grob *me, Axis a)
{
  Stencil *s = unsmob_stencil (Accidental_interface::get_stencil (me));

  if (s)
    return ly_interval2scm (s->extent (a));
  return ly_interval2scm (Interval ());
}

MAKE_SCHEME_CALLBACK (Accidental_interface, height, 1);
SCM
Accidental_interface::height (SCM smob)
{
  return get_extent (unsmob_grob (smob), Y_AXIS);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, width, 1);
SCM
Accidental_interface::width (SCM smob)
{
  return get_extent (unsmob_grob (smob), X_AXIS);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, pure_height, 3);
SCM
Accidental_interface::pure_height (SCM smob, SCM start_scm, SCM)
{
  Item *me = dynamic_cast<Item*> (unsmob_grob (smob));
  int start = scm_to_int (start_scm);
  int rank = me->get_column ()->get_rank ();

  if (to_boolean (me->get_property ("forced"))
      || !unsmob_grob (me->get_object ("tie"))
      || rank == start + 1) /* we are at the start of a line */
    {
      Stencil *s = unsmob_stencil (get_stencil (me));
      if (s)
	return ly_interval2scm (s->extent (Y_AXIS));
    }

  return ly_interval2scm (Interval ());
}

vector<Box>
Accidental_interface::accurate_boxes (Grob *me, Grob **common)
{
  Box b;
  b[X_AXIS] = me->extent (me, X_AXIS);
  b[Y_AXIS] = me->extent (me, Y_AXIS);

  vector<Box> boxes;

  bool parens = to_boolean (me->get_property ("parenthesized"));
  if (!me->is_live ())
    return boxes;

  if (!to_boolean (me->get_property ("restore-first"))
      && !parens)
    {
      SCM alist = me->get_property ("glyph-name-alist");
      SCM alt = me->get_property ("alteration");
      string glyph_name = robust_scm2string (ly_assoc_get (alt, alist, SCM_BOOL_F),
					     "");
      
      if (glyph_name == "accidentals.flat"
	  || glyph_name == "accidentals.mirroredflat")
	{
	  Box stem = b;
	  Box bulb = b;

	  /*
	    we could make the stem thinner, but that places the flats
	    really close.
	  */
	  Direction bulb_dir =
	    glyph_name == "accidentals.mirroredflat" ? LEFT : RIGHT;
	  stem[X_AXIS][bulb_dir] = stem[X_AXIS].center ();

	  /*
	    To prevent vertical alignment for 6ths
	  */
	  stem[Y_AXIS] *= 1.1;
	  bulb[Y_AXIS][UP] *= .35;

	  boxes.push_back (bulb);
	  boxes.push_back (stem);
	}
      else if (glyph_name ==  "accidentals.natural")
	{
	  Box lstem = b;
	  Box rstem = b;
	  Box belly = b;

	  lstem[Y_AXIS] *= 1.1;
	  rstem[Y_AXIS] *= 1.1;

	  belly[Y_AXIS] *= 0.75;
	  lstem[X_AXIS][RIGHT] *= .33;
	  rstem[X_AXIS][LEFT] = rstem[X_AXIS].linear_combination (1.0 / 3.0);
	  lstem[Y_AXIS][DOWN] = belly[Y_AXIS][DOWN];
	  rstem[Y_AXIS][UP] = belly[Y_AXIS][UP];
	  boxes.push_back (belly);
	  boxes.push_back (lstem);
	  boxes.push_back (rstem);
	}
      /*
	TODO: add support for, double flat.
      */
    }

  if (!boxes.size ())
    boxes.push_back (b);

  Offset o (me->relative_coordinate (common[X_AXIS], X_AXIS),
	    me->relative_coordinate (common[Y_AXIS], Y_AXIS));

  for (vsize i = boxes.size (); i--;)
    boxes[i].translate (o);

  return boxes;
}

MAKE_SCHEME_CALLBACK (Accidental_interface, print, 1);
SCM
Accidental_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *tie = unsmob_grob (me->get_object ("tie"));

  if (tie && !tie->original ()
      && !to_boolean (me->get_property ("forced")))
    {
      me->suicide ();
      return SCM_EOL;
    }

  return get_stencil (me);
}

SCM
Accidental_interface::get_stencil (Grob *me)
{
  Font_metric *fm = Font_interface::get_default_font (me);

  SCM alist = me->get_property ("glyph-name-alist");
  SCM alt = me->get_property ("alteration");
  SCM glyph_name = ly_assoc_get (alt, alist, SCM_BOOL_F);
  
  if (!scm_is_string (glyph_name))
    {
      me->warning (_f ("Could not find glyph-name for alteration %s",
		       ly_scm_write_string (alt).c_str ()));
      return SCM_EOL;
    }
  
  Stencil mol (fm->find_by_name (ly_scm2string (glyph_name)));
  if (to_boolean (me->get_property ("restore-first")))
    {
      /*
	this isn't correct for ancient accidentals, but they don't
	use double flats/sharps anyway.
	*/
      Stencil acc (fm->find_by_name ("accidentals.natural"));

      if (acc.is_empty ())
	me->warning (_ ("natural alteration glyph not found"));
      else
	mol.add_at_edge (X_AXIS, LEFT, acc, 0.1);
    }
  
  if (to_boolean (me->get_property ("parenthesized")))
    mol = parenthesize (me, mol);

  return mol.smobbed_copy ();
}

  
ADD_INTERFACE (Accidental_interface,
	       "A single accidental.",
	       
	       /* properties */
	       "alteration "
	       "avoid-slur "
	       "forced "
	       "parenthesized "
	       "restore-first "
	       "glyph-name-alist "
	       "tie "
	       );
