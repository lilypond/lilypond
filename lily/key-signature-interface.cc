/*
  key-item.cc -- implement Key_signature_interface

  source file of the GNU LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

  DECLARE_GROB_INTERFACE();
};


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

  Stencil mol;

  SCM c0s = me->get_property ("c0-position");

  bool is_cancellation = me->internal_has_interface
    (ly_symbol2scm ("key-cancellation-interface"));

  /*
    SCM lists are stacks, so we work from right to left, ending with
    the cancellation signature.
  */

  int last_pos = -1000;
  Font_metric *fm = Font_interface::get_default_font (me);
  SCM alist = me->get_property ("glyph-name-alist");

  for (SCM s = me->get_property ("alteration-alist"); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM alt = is_cancellation
	? scm_from_int (0) 
	: scm_cdar (s);

      SCM glyph_name = ly_assoc_get (alt, alist, SCM_BOOL_F);
      Stencil acc (fm->find_by_name (ly_scm2string (glyph_name)));

      if (acc.is_empty ())
	me->warning (_ ("alteration not found"));
      else
	{
	  SCM what = scm_caar (s);

	  SCM proc = ly_lily_module_constant ("key-signature-interface::alteration-position");

	  int pos = scm_to_int (scm_call_3 (proc, what, scm_cdar (s), c0s));
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

ADD_INTERFACE (Key_signature_interface,
	       "A group of accidentals, to be printed as signature sign.",

	       "alteration-alist "
	       "c0-position "
	       "glyph-name-alist "
	       "style "
	       );
