/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c)  1999--2001 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "paper-column.hh"
#include "line-of-score.hh"
#include "staff-symbol-referencer.hh"
#include "text-item.hh"

MAKE_SCHEME_CALLBACK (Chord_name,after_line_breaking,1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item* me = dynamic_cast<Item*> (unsmob_grob (smob));
  assert (me);
    
  SCM s = me->get_grob_property ("begin-of-line-visible");
  if (to_boolean (s))
    {
      if (Paper_column::rank_i (me->column_l ()) -
	  /*
	    hmm, what's my column number in this line?
	    why doesn't this work?
	    me->line_l ()->rank_i_ > 2)
	  */
	  me->line_l ()->spanned_rank_iv ()[LEFT] > 1)
	me->suicide ();
    }
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Chord_name,brew_molecule,1);
SCM
Chord_name::brew_molecule (SCM smob) 
{
  Grob *me = unsmob_grob (smob);
  SCM style = me->get_grob_property ("style");

  if (!gh_symbol_p (style))
    style = ly_symbol2scm ("banter");

  SCM chord = me-> get_grob_property ("chord");
  SCM func = me->get_grob_property (ly_symbol2scm ("chord-name-function"));
  SCM text = gh_call2 (func, style, chord);

  SCM properties = Font_interface::font_alist_chain (me);
  Molecule mol = Text_item::text2molecule (me, text, properties);

  SCM space =  me->get_grob_property ("word-space");
  if (gh_number_p (space))
    {
      Molecule m;
      m.set_empty (false);
      mol.add_at_edge (X_AXIS, RIGHT, m, gh_scm2double (space)*
		       Staff_symbol_referencer::staff_space (me));
    }

  return mol.smobbed_copy ();
}
