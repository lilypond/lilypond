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

// -> offset callback
MAKE_SCHEME_CALLBACK(Rest,after_line_breaking);
SCM
Rest::after_line_breaking (SCM smob)
{
  Score_element *me = unsmob_element (smob);
  int bt = gh_scm2int (me->get_elt_property ("duration-log"));
  if (bt == 0)
    {

      me->translate_axis (Staff_symbol_referencer::staff_space (me) , Y_AXIS);
    }

  Score_element * d = unsmob_element (me->get_elt_property ("dot"));
  if (d && bt > 4) // UGH.
    {
      d->set_elt_property ("staff-position",
			   gh_int2scm ((bt == 7) ? 4 : 3));
    }

  return SCM_UNDEFINED;
}


MAKE_SCHEME_CALLBACK(Rest,brew_molecule)
SCM 
Rest::brew_molecule (SCM smob) 
{
  Score_element* me = unsmob_element (smob);
  
  bool ledger_b =false;

  SCM balltype = me->get_elt_property ("duration-log");
  
  if (balltype == gh_int2scm (0) || balltype == gh_int2scm (1))
    {
      int sz = Staff_symbol_referencer::line_count (me);
      Real dif = abs(Staff_symbol_referencer::position_f (me)  - (2* gh_scm2int (balltype) - 1)); 
      ledger_b = dif > sz;
    }
  
  String style; 
  SCM style_sym =me->get_elt_property ("style");
  if (gh_scm2int (balltype) >= 2 && gh_string_p (style_sym))
    {
      style = ly_scm2string (style_sym);
    }

  String idx =  ("rests-") + to_str (gh_scm2int (balltype))
    + (ledger_b ? "o" : "") + style;

  return me-> lookup_l ()->afm_find (idx).create_scheme();
}


bool
Rest::has_interface (Score_element*m)
{
  return m && m->has_interface (ly_symbol2scm ("rest-interface"));
}
