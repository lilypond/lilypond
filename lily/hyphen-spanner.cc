/*
  hyphen-spanner.cc -- implement Hyphen_spanner

  source file of the GNU LilyPond music typesetter

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>

  (adapted from extender-spanner)
*/

#include <math.h>

#include "box.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "hyphen-spanner.hh"
#include "spanner.hh"
#include "item.hh"

MAKE_SCHEME_SCORE_ELEMENT_NON_DEFAULT_CALLBACKS(Hyphen_spanner)

SCM 
Hyphen_spanner::scheme_molecule (SCM smob)
{
  Spanner * sp  =dynamic_cast<Spanner*> (unsmob_element (smob));
  Molecule  mol;

  Real leftext = sp->get_bound (LEFT)->extent (X_AXIS).length ();

  Real ss = sp->paper_l ()->get_var ("staffspace");
  Real lt = sp->paper_l ()->get_var ("stafflinethickness");
  Real th = gh_scm2double (sp->get_elt_property ("thickness")) * lt ;
  Real h = gh_scm2double (sp->get_elt_property ("height")) * ss;
  Real l = gh_scm2double (sp->get_elt_property ("minimum-length")) * ss;  
  Real w = sp->spanner_length () - leftext - ss/2; 


  /* First try: just make the hyphen take 1/3 of the available space  
   for length, use a geometric mean of the available space and some minimum
  */
  if(l < w)
    l = sqrt(l*w);

  Box b  (Interval ( (w-l)/2, (w+l)/2), Interval (h,h+th));
  mol.add_molecule (sp->lookup_l ()->filledbox (b));
  mol.translate_axis(leftext, X_AXIS);
  return mol.create_scheme ();
}
  
void
Hyphen_spanner::set_textitem (Direction d, Item* textitem_l)
{
  elt_l_->set_bound (d, textitem_l);
  elt_l_->add_dependency (textitem_l);
}

Hyphen_spanner::Hyphen_spanner (Spanner*s)
{
  elt_l_ = s;
}
