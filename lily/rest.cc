/*
 rest.cc -- implement Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "rest.hh"
#include "dots.hh"
#include "paper-score.hh"
#include "staff-symbol-referencer.hh"


void
Rest::after_line_breaking ()
{
  if (balltype_i () == 0)
    {
      Staff_symbol_referencer_interface si (this);
      si.set_position (si.position_f () + 2);
    }
  
  Dots * d = dots_l ();
  if (d && balltype_i () > 4) // UGH.
    {
      /*
	UGH. 
       */
      staff_symbol_referencer (d)
	.set_position ((balltype_i () == 7) ? 4 : 3);
    }
}


MAKE_SCHEME_SCORE_ELEMENT_CALLBACKS(Rest)
Molecule 
Rest::do_brew_molecule () const
{
  bool ledger_b =false;

  if (balltype_i () == 0 || balltype_i () == 1)
    {
      Staff_symbol_referencer_interface si(this);
      ledger_b = abs(si.position_f ()  - (2* balltype_i () - 1))
	> si.line_count (); 
    }
  
  String style; 
  SCM style_sym =get_elt_property ("style");
  if (balltype_i () >= 2 &&gh_string_p ( style_sym))
    {
      style = ly_scm2string (style_sym);
    }

  String idx =  ("rests-") + to_str (balltype_i ()) + (ledger_b ? "o" : "") + style;

  return lookup_l ()->afm_find (idx);
}



Rest::Rest (SCM s)
  : Rhythmic_head (s)
{
}

