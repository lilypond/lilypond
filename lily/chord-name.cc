/*
  chord-name.cc -- implement Chord_name

  source file of the GNU LilyPond music typesetter

  (c)  1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord-name.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "score-element.hh"
#include "paper-column.hh"
#include "line-of-score.hh"
#include "staff-symbol-referencer.hh"
#include "text-item.hh"

MAKE_SCHEME_CALLBACK (Chord_name,after_line_breaking,1);
SCM
Chord_name::after_line_breaking (SCM smob)
{
  Item* me = dynamic_cast<Item*> (unsmob_element (smob));
  assert (me);
    
  SCM s = me->get_elt_property ("begin-of-line-visible");
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
  Score_element *me = unsmob_element (smob);
  SCM style = me->get_elt_property ("style");

  if (!gh_string_p (style))
    style = ly_str02scm ("banter");

  SCM inversion = me-> get_elt_property ("inversion");
  if (inversion == SCM_EOL)
    inversion = SCM_BOOL_F;

  SCM bass =  me->get_elt_property ("bass");
  if (bass == SCM_EOL)
    bass = SCM_BOOL_F;

  SCM pitches =  me->get_elt_property ("pitches");
  SCM func = me->get_elt_property (ly_symbol2scm ("chord-name-function"));
  SCM text = gh_call3 (func, style, pitches, gh_cons (inversion, bass));

  SCM properties = gh_append2 (me->immutable_property_alist_,
			       me->mutable_property_alist_);
  Molecule mol = Text_item::text2molecule (me, text, properties);

  SCM space =  me->get_elt_property ("word-space");
  if (gh_number_p (space))
    {
      Molecule m;
      m.set_empty (false);
      mol.add_at_edge (X_AXIS, RIGHT, m, gh_scm2double (space)*
		       Staff_symbol_referencer::staff_space (me));
    }

  return mol.create_scheme ();
}
