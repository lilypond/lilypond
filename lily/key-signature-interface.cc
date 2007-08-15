/*
  key-item.cc -- implement Key_signature_interface

  source file of the GNU LilyPond music typesetter

  (c) 1996--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>

  keyplacement by Mats Bengtsson
*/

#include "accidental-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"

struct Key_signature_interface
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  static bool has_interface (Grob *);
};

/*
  FIXME: too much hardcoding here.
*/
const int FLAT_TOP_PITCH = 2; /* fes, ges, as and bes typeset in lower octave */
const int SHARP_TOP_PITCH = 4; /*  ais and bis typeset in lower octave */

/*
  TODO: look this up. I'm not sure where the naturals ought to go.

  COMMENT: Current implementation does not use the NATURAL_TOP_PITCH for anything,
           always typesets naturals in the same place as the thing they cancel. -rz
*/
const int NATURAL_TOP_PITCH = 4;

/*
  FIXME: key-item should just get a list of (position, acc), and leave
  the thinking to other parties.

  - TODO: put this in Scheme

  TODO: can  we do without c0pos? it's partly musical.
*/
int
alteration_pos (SCM what, int alter, int c0p)
{
  if (scm_is_pair (what))
    return scm_to_int (scm_car (what)) * 7 + scm_to_int (scm_cdr (what)) + c0p;

  int p = scm_to_int (what);

  // Find the c in the range -4 through 2
  int from_bottom_pos = c0p + 4;
  from_bottom_pos = from_bottom_pos % 7;
  from_bottom_pos = (from_bottom_pos + 7) % 7; // Precaution to get positive.
  int c0 = from_bottom_pos - 4;

  if ((alter < 0 && ((p > FLAT_TOP_PITCH) || (p + c0 > 4)) && (p + c0 > 1))
      || (alter > 0 && ((p > SHARP_TOP_PITCH) || (p + c0 > 5)) && (p + c0 > 2))
      || (alter == 0 && ((p > NATURAL_TOP_PITCH) || (p + c0 > 5)) && (p + c0 > 2)))
    {
      p -= 7; /* Typeset below c_position */
    }

  /* Provide for the four cases in which there's a glitch
     it's a hack, but probably not worth
     the effort of finding a nicer solution.
     --dl. */
  if (c0 == 2 && alter > 0 && p == 3)
    p -= 7;
  if (c0==-3 && alter > 0 && p ==-1)
    p += 7;
  if (c0==-4 && alter < 0 && p ==-1)
    p += 7;
  if (c0==-2 && alter < 0 && p ==-3)
    p += 7;

  return p + c0;
}

/*
  TODO
  - space the `natural' signs wider
*/
MAKE_SCHEME_CALLBACK (Key_signature_interface, print, 1);
SCM
Key_signature_interface::print (SCM smob)
{
  Item *me = dynamic_cast<Item *> (unsmob_grob (smob));

  Real inter = Staff_symbol_referencer::staff_space (me) / 2.0;

  SCM scm_style = me->get_property ("style");
  string style;
  if (scm_is_symbol (scm_style))
    style = ly_symbol2string (scm_style);
  else
    style = "";

  SCM newas = me->get_property ("alteration-alist");
  Stencil mol;

  SCM c0s = me->get_property ("c0-position");
  int c0p = 0;
  if (scm_is_number (c0s))
    c0p = scm_to_int (c0s);

  bool is_cancellation = me->internal_has_interface
    (ly_symbol2scm ("key-cancellation-interface"));

  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */

  int last_pos = -1000;
  Font_metric *fm = Font_interface::get_default_font (me);
  for (SCM s = newas; scm_is_pair (s); s = scm_cdr (s))
    {
      int alteration = scm_to_int (scm_cdar (s));
      string font_char
	= Accidental_interface::get_fontcharname (style,
						  is_cancellation
						  ? 0
						  : alteration);
      Stencil acc (fm->find_by_name ("accidentals." + font_char));

      if (acc.is_empty ())
	me->warning (_f ("accidental `%s' not found", font_char));
      else
	{
	  SCM what = scm_caar (s);
	  int pos = alteration_pos (what, alteration, c0p);
	  acc.translate_axis (pos * inter, Y_AXIS);

	  /*
	    The natural sign (unlike flat & sharp)
	    has vertical edges on both sides. A little padding is
	    needed to prevent collisions.
	  */
	  Real padding = 0.0;
	  if (is_cancellation
	      && last_pos < pos + 2
	      && last_pos > pos - 6)
	    padding = 0.3;

	  mol.add_at_edge (X_AXIS, LEFT, acc, padding, 0);
	  last_pos = pos;
	}
    }

  mol.align_to (X_AXIS, LEFT);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Key_signature_interface, "key-signature-interface",
	       "A group of accidentals, to be printed as signature sign.",
	       "style c0-position alteration-alist");
