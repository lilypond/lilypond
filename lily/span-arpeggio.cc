/*
  span-arpeggio.cc -- implement Span_arpeggio

  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "axis-group-interface.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "arpeggio.hh"
#include "span-arpeggio.hh"
#include "score-element.hh"
#include "stem.hh"
#include "staff-symbol-referencer.hh"

bool
Span_arpeggio::has_interface (Score_element* me)
{
  return me && me->has_interface (ly_symbol2scm ("span-arpeggio-interface"));
}

/*
  We could collapse this with Arpeggio::brew_molecule, but that requires
  hairy scm callback hacking.
 */
MAKE_SCHEME_CALLBACK (Span_arpeggio, brew_molecule, 1);
SCM 
Span_arpeggio::brew_molecule (SCM smob) 
{
  Score_element *me = unsmob_element (smob);
  
  Interval iv;
  Score_element *common = me;
  for (SCM s = me->get_elt_property ("arpeggios"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element *arpeggio = unsmob_element (gh_car (s));
      common = arpeggio->common_refpoint (common, Y_AXIS);
    }
  // Hmm, nothing in common?
  if (0) //common)
    for (SCM s = me->get_elt_property ("arpeggios"); gh_pair_p (s); s = gh_cdr (s))
      {
	Score_element *arpeggio = unsmob_element (gh_car (s));
	Real c = common->relative_coordinate (arpeggio, Y_AXIS);
	//iv.unite (Arpeggio::head_positions (stem));
	iv.unite (Interval (c, c));
      }
  else
    iv = Interval (-23, 5);

  Molecule mol;
  Molecule arpeggio = me->paper_l ()->lookup_l (0)->afm_find ("scripts-arpeggio");
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  for (int i = (int)iv[MIN]/ 2; i < (int)(iv[MAX] - 1)/ 2; i++)
    {
      Molecule a (arpeggio);
      a.translate_axis (i * staff_space, Y_AXIS);
      mol.add_molecule (a);
    }
  mol.translate (Offset (-2 * staff_space, 0));

  return mol.create_scheme (); 
}

