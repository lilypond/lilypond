/*
  key-item.cc -- implement Key_signature_interface

  source file of the GNU LilyPond music typesetter

  (c) 1996--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>

  keyplacement by Mats Bengtsson
*/

#include "accidental-interface.hh"
#include "font-interface.hh"
#include "international.hh"
#include "item.hh"
#include "lookup.hh"
#include "output-def.hh"
#include "staff-symbol-referencer.hh"
#include "rational.hh"

struct Key_signature_interface
{
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE ();
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
  SCM last_glyph_name = SCM_BOOL_F;
  SCM padding_pairs = me->get_property ("padding-pairs");
    
  Font_metric *fm = Font_interface::get_default_font (me);
  SCM alist = me->get_property ("glyph-name-alist");

  for (SCM s = me->get_property ("alteration-alist"); scm_is_pair (s); s = scm_cdr (s))
    {
      SCM alt = is_cancellation
	? scm_from_int (0)
	: scm_cdar (s);

      SCM glyph_name_scm = ly_assoc_get (alt, alist, SCM_BOOL_F);
      if (!scm_is_string (glyph_name_scm))
	{
	  me->warning (_f ("No glyph found for alteration: %s",
			   ly_scm2rational (alt).to_string ().c_str ()));
	  continue;
	}

      string glyph_name = ly_scm2string (glyph_name_scm);

      Stencil acc (fm->find_by_name (glyph_name));

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
	  Real padding = robust_scm2double (me->get_property ("padding"),
					    0.0);
	  SCM handle = scm_assoc (scm_cons (glyph_name_scm, last_glyph_name),
				  padding_pairs);
	  if (scm_is_pair (handle))
	    padding = robust_scm2double (scm_cdr (handle), 0.0);
	  else if (glyph_name ==  "accidentals.natural"
	      && last_pos < pos + 2
	      && last_pos > pos - 6)
	    padding += 0.3;

	  mol.add_at_edge (X_AXIS, LEFT, acc, padding);
	  
	  last_pos = pos;
	  last_glyph_name = glyph_name_scm;
	}
    }

  mol.align_to (X_AXIS, LEFT);

  return mol.smobbed_copy ();
}

ADD_INTERFACE (Key_signature_interface,
	       "A group of accidentals, to be printed as signature sign.",

	       /* properties */
	       "alteration-alist "
	       "c0-position "
	       "glyph-name-alist "
	       "padding "
	       "padding-pairs "
	       "style "
	       );
