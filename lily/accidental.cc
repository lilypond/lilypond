/*
  accidental.cc -- implement Accidental_interface

  (c) 2001--2004 Han-Wen Nienhuys
  
 */
#include "font-interface.hh"
#include "item.hh"
#include "stencil.hh"
#include "accidental-interface.hh"
#include "paper-def.hh"
#include "pitch.hh"

/*
  TODO: insert support for smaller cautionaries, tie-break-reminders.
  Either here or in new-accidental-engraver.

  'accidentals should go, for a single 'accidental property -- see
  accidental-placement.cc

*/


Stencil
parenthesize (Grob*me, Stencil m)
{
  Stencil open = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-leftparen"));
  Stencil close = Font_interface::get_default_font (me)->find_by_name (String ("accidentals-rightparen"));
  m.add_at_edge (X_AXIS, LEFT, Stencil (open), 0,0);
  m.add_at_edge (X_AXIS, RIGHT, Stencil (close), 0,0);

  return m;
}


MAKE_SCHEME_CALLBACK (Accidental_interface,after_line_breaking,1);
SCM
Accidental_interface::after_line_breaking (SCM smob)
{
  Grob *me  = unsmob_grob (smob);
  Grob *tie = unsmob_grob (me->get_property ("tie"));

  if (tie && !tie->original_)
    {
      me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

Array<Box>
Accidental_interface::accurate_boxes (Grob *a,Grob**common)
{
  Box b;
  b[X_AXIS] = a->extent (a, X_AXIS);
  b[Y_AXIS] = a->extent (a, Y_AXIS);

  Array<Box> boxes;
  
  bool parens = false;
  if (to_boolean (a->get_property ("cautionary")))
    {
      SCM cstyle = a->get_property ("cautionary-style");
      parens = is_equal (cstyle, ly_symbol2scm ("parentheses"));

    }

  SCM accs = a->get_property ("accidentals");
  SCM scm_style = a->get_property ("style");
  if (!is_symbol (scm_style)
      && !parens
      && scm_ilength (accs) == 1)
    {
      if (ly_scm2int (ly_car (accs)) == FLAT)
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
	  stem[Y_AXIS]  *= 1.1;  
	  bulb[Y_AXIS][UP] *= .35;

	  boxes.push (bulb);
	  boxes.push (stem);
	}
      
      /*
	TODO: add support for natural, double flat.
       */
    }

  if (!boxes.size ())
    boxes.push (b);

  Offset o (a->relative_coordinate (common[X_AXIS],  X_AXIS),
	    a->relative_coordinate (common[Y_AXIS],  Y_AXIS));
  for (int i = boxes.size (); i--;)
    {
      boxes[i].translate (o);
    }
  
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
String
Accidental_interface::get_fontcharname (String style, int alteration)
{
  if (alteration == DOUBLE_FLAT
      || alteration == DOUBLE_SHARP)
    {
      return to_string (alteration);
    }
  
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
  
  if (style == "neo_mensural")
    style = ""; // currently same as default
  if (style == "default")
    style = "";
  return style + to_string (alteration);
}

MAKE_SCHEME_CALLBACK (Accidental_interface,print,1);
SCM
Accidental_interface::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  bool smaller = false;
  bool parens = false;

  bool caut  = to_boolean (me->get_property ("cautionary"));
  if (caut)
    {
      SCM cstyle = me->get_property ("cautionary-style");
      parens = is_equal (cstyle, ly_symbol2scm ("parentheses"));
      smaller = is_equal (cstyle, ly_symbol2scm ("smaller"));
    }

  SCM scm_style = me->get_property ("style");
  String style;
  if (is_symbol (scm_style))
    {
      style = ly_symbol2string (scm_style);
    }
  else
    {
      /*
	preferably no name for the default style.
      */
      style = "";
    }

  Font_metric *fm = 0;
  if (smaller)
    {
      SCM ac = Font_interface::text_font_alist_chain (me);
      ac = scm_cons (scm_cons (scm_cons
			     (ly_symbol2scm ("font-size"),
			      scm_int2num (-2)), SCM_EOL),
		    ac);
      fm = select_font (me->get_paper (), ac);
    }
  else
    fm = Font_interface::get_default_font (me);

  Stencil mol;
  for (SCM s = me->get_property ("accidentals");
       ly_c_pair_p (s); s = ly_cdr (s))
    {
      int alteration = ly_scm2int (ly_car (s));
      String font_char = get_fontcharname (style, alteration);
      Stencil acc (fm->find_by_name ("accidentals-" + font_char));

      if (acc.is_empty ())
	{
	  me->warning (_f ("accidental `%s' not found", font_char));
	}
      else
	{
	  mol.add_at_edge (X_AXIS,  RIGHT, acc, 0.1,0);
	}
    }

  if (parens)
    mol = parenthesize (me, mol); 

  return mol.smobbed_copy ();
}



ADD_INTERFACE (Accidental_interface, "accidental-interface",
	      "a single accidental",
	       "cautionary cautionary-style style tie accidentals");
