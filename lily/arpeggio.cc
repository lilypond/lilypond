/*   
  arpegggio.cc -- implement Arpeggio

  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "arpeggio.hh"
#include "score-element.hh"
#include "stem.hh"
#include "staff-symbol-referencer.hh"

bool
Arpeggio::has_interface (Score_element* me)
{
  return me && me->has_interface (ly_symbol2scm ("arpeggio-interface"));
}

MAKE_SCHEME_CALLBACK (Arpeggio, brew_molecule, 1);
SCM 
Arpeggio::brew_molecule (SCM smob) 
{
  Score_element *me = unsmob_element (smob);
  
  Interval iv; 
  for (SCM s = me->get_elt_property ("stems"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element *stem = unsmob_element (gh_car (s));
      iv.unite (Stem::head_positions (stem));
    }

  Molecule mol;
  Molecule dot = me->paper_l ()->lookup_l (0)->afm_find ("dots-dot");
  Real half_space = Staff_symbol_referencer::staff_space (me) / 2;
  for (Real i = iv[MIN]; i <= iv[MAX]; i++)
    {
      Molecule d (dot);
      d.translate_axis (i * half_space, Y_AXIS);
      mol.add_molecule (d);
    }
  mol.translate (Offset (-3, 0));

  return mol.create_scheme (); 
}


