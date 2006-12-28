/*
  accidental.cc -- implement Accidental_interface

  source file of the GNU LilyPond music typesetter

  (c) 2001--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "accidental-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "output-def.hh"
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
Accidental_interface::accurate_boxes (Grob *me, Grob **common)
{
  Box b;
  b[X_AXIS] = me->extent (me, X_AXIS);
  b[Y_AXIS] = me->extent (me, Y_AXIS);

  vector<Box> boxes;

  bool parens = to_boolean (me->get_property ("parenthesized"));

  SCM scm_style = me->get_property ("style");
  if (!scm_is_symbol (scm_style)
      && !to_boolean (me->get_property ("restore-first"))
      && !parens)
    {
      int acc = scm_to_int (me->get_property ("alteration"));
      switch (acc)
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

  Offset o (me->relative_coordinate (common[X_AXIS], X_AXIS),
	    me->relative_coordinate (common[Y_AXIS], Y_AXIS));

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
  bool parens = to_boolean (me->get_property ("parenthesized"));

  SCM scm_style = me->get_property ("style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    /*
      preferably no name for the default style.
    */
    style = "";

  Font_metric *fm = Font_interface::get_default_font (me);

  SCM stencils = me->get_property ("stencils");
  if (!scm_is_pair (stencils)
      || !unsmob_stencil (scm_car (stencils)))
    return SCM_EOL;
  
  Stencil mol (*unsmob_stencil (scm_car (stencils)));
  if (to_boolean (me->get_property ("restore-first")))
    {
      string font_char = get_fontcharname (style, 0);
      Stencil acc (fm->find_by_name ("accidentals." + font_char));

      if (acc.is_empty ())
	me->warning (_f ("accidental `%s' not found", font_char));
      else
	mol.add_at_edge (X_AXIS, LEFT, acc, 0.1, 0);
    }
  
  if (parens)
    mol = parenthesize (me, mol);

  return mol.smobbed_copy ();
}
  
MAKE_SCHEME_CALLBACK (Accidental_interface, calc_stencils, 1);
SCM
Accidental_interface::calc_stencils (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  SCM scm_style = me->get_property ("style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    /*
      preferably no name for the default style.
    */
    style = "";


  Font_metric *fm = Font_interface::get_default_font (me);
  SCM acc = me->get_property ("alteration");
  if (scm_is_number (acc))
    {
      string font_char = get_fontcharname (style, scm_to_int (acc));
      
      Stencil acc_stencil (fm->find_by_name ("accidentals." + font_char));

      return scm_list_1 (acc_stencil.smobbed_copy ());
    }
  else
    return SCM_EOL;
}

  
ADD_INTERFACE (Accidental_interface,
	       "a single accidental",
	       
	       /* props */
	       "alteration "
	       "avoid-slur "
	       "forced "
	       "style "
	       "parenthesized " 
	       "tie "
	       );
