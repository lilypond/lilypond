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
#include "pitch.hh"
#include "stencil.hh"

/*
  TODO: insert support for smaller cautionaries, tie-break-reminders.
  Either here or in new-accidental-engraver.

  'accidentals should go, for a single 'accidental property -- see
  accidental-placement.cc
*/
Stencil
parenthesize (Grob *me, Stencil m)
{
  Font_metric * font
    = Font_interface::get_default_font (me); 
  Stencil open
    = font->find_by_name ("accidentals.leftparen");
  Stencil close
    = font->find_by_name ("accidentals.rightparen");

  m.add_at_edge (X_AXIS, LEFT, Stencil (open), 0, 0);
  m.add_at_edge (X_AXIS, RIGHT, Stencil (close), 0, 0);

  return m;
}

/*
  Hmm. Need separate callback, or perhaps #'live bool property.
 */
MAKE_SCHEME_CALLBACK (Accidental_interface, after_line_breaking, 1);
SCM
Accidental_interface::after_line_breaking (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *tie = unsmob_grob (me->get_object ("tie"));

  if (tie && !tie->original ()
      && !to_boolean (me->get_property ("forced")))
    {
      me->suicide ();
    }
 
  return SCM_UNSPECIFIED;
}

vector<Box>
Accidental_interface::accurate_boxes (Grob *a, Grob **common)
{
  Box b;
  b[X_AXIS] = a->extent (a, X_AXIS);
  b[Y_AXIS] = a->extent (a, Y_AXIS);

  vector<Box> boxes;

  bool parens = false;
  if (to_boolean (a->get_property ("cautionary")))
    {
      SCM cstyle = a->get_property ("cautionary-style");
      parens = ly_is_equal (cstyle, ly_symbol2scm ("parentheses"));
    }

  SCM accs = a->get_property ("accidentals");
  SCM scm_style = a->get_property ("style");
  if (!scm_is_symbol (scm_style)
      && !parens
      && scm_ilength (accs) == 1)
    {
      switch (scm_to_int (scm_car (accs)))
	{
	case FLAT:
	  {
	    Box stem = b;
	    Box bulb = b;

	    /*
	      we could make the stem thinner, but that places the flats
	      really close.
	    */
	    stem[X_AXIS][RIGHT] *= .5;

	    /*
	      To prevent vertical alignment for 6ths
	    */
	    stem[Y_AXIS] *= 1.1;
	    bulb[Y_AXIS][UP] *= .35;

	    boxes.push_back (bulb);
	    boxes.push_back (stem);
	  }
	  break;
	case NATURAL:
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
	  break;
	  /*
	    TODO: add support for, double flat.
	  */
	}
    }

  if (!boxes.size ())
    boxes.push_back (b);

  Offset o (a->relative_coordinate (common[X_AXIS], X_AXIS),
	    a->relative_coordinate (common[Y_AXIS], Y_AXIS));
  for (vsize i = boxes.size (); i--;)
    boxes[i].translate (o);

  return boxes;
}

/*
 * Some styles do not provide all flavours of accidentals, e.g. there
 * is currently no sharp accidental in vaticana style.  In these cases
 * this function falls back to one of the other styles.
 */

/*
  todo: this sort of stuff in Scheme. --hwn.
*/
string
Accidental_interface::get_fontcharname (string style, int alteration)
{
  if (alteration == DOUBLE_FLAT
      || alteration == DOUBLE_SHARP)
    return to_string (alteration);

  if (style == "hufnagel")
    switch (alteration)
      {
      case FLAT: return "hufnagel-1";
      case 0: return "vaticana0";
      case SHARP: return "mensural1";
      }
  if (style == "medicaea")
    switch (alteration)
      {
      case FLAT: return "medicaea-1";
      case 0: return "vaticana0";
      case SHARP: return "mensural1";
      }
  if (style == "vaticana")
    switch (alteration)
      {
      case FLAT: return "vaticana-1";
      case 0: return "vaticana0";
      case SHARP: return "mensural1";
      }
  if (style == "mensural")
    switch (alteration)
      {
      case FLAT: return "mensural-1";
      case 0: return "vaticana0";
      case SHARP: return "mensural1";
      }

  if (style == "neomensural")
    style = ""; // currently same as default
  if (style == "default")
    style = "";
  return style + to_string (alteration);
}

MAKE_SCHEME_CALLBACK (Accidental_interface, print, 1);
SCM
Accidental_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  bool smaller = false;
  bool parens = false;

  bool caut = to_boolean (me->get_property ("cautionary"));
  if (caut)
    {
      SCM cstyle = me->get_property ("cautionary-style");
      parens = ly_is_equal (cstyle, ly_symbol2scm ("parentheses"));
      smaller = ly_is_equal (cstyle, ly_symbol2scm ("smaller"));
    }

  SCM scm_style = me->get_property ("style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    /*
      preferably no name for the default style.
    */
    style = "";

  Font_metric *fm = 0;
  if (smaller)
    {
      SCM ac = Font_interface::music_font_alist_chain (me);
      /*
	TODO: should calc font-size by adding -2 to current font-size
      */
      ac = scm_cons (scm_list_1 (scm_cons
				 (ly_symbol2scm ("font-size"),
				  scm_from_int (-2))),
		     ac);
      fm = select_font (me->layout (), ac);
    }
  else
    fm = Font_interface::get_default_font (me);

  Stencil mol;
  for (SCM s = me->get_property ("accidentals");
       scm_is_pair (s); s = scm_cdr (s))
    {
      int alteration = scm_to_int (scm_car (s));
      string font_char = get_fontcharname (style, alteration);
      Stencil acc (fm->find_by_name ("accidentals." + font_char));

      if (acc.is_empty ())
	me->warning (_f ("accidental `%s' not found", font_char));
      else
	mol.add_at_edge (X_AXIS, RIGHT, acc, 0.1, 0);
    }

  if (parens)
    mol = parenthesize (me, mol);

  return mol.smobbed_copy ();
}

/*
  TODO: should move avoid-slur into item?
*/
ADD_INTERFACE (Accidental_interface, "accidental-interface",
	       "a single accidental",
	       "accidentals "
	       "avoid-slur "
	       "cautionary "
	       "cautionary-style "
	       "forced "
	       "style "
	       "tie "
	       );
