/*
  staffsym.cc -- implement Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "staff-symbol.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "debug.hh"
#include "item.hh"



Molecule 
Staff_symbol::do_brew_molecule() const
{
  Score_element * common
    = spanned_drul_[LEFT]->common_refpoint (spanned_drul_[RIGHT], X_AXIS);
  
  Real width =
    // right_shift     - left_shift
    + spanned_drul_[RIGHT]->relative_coordinate (common , X_AXIS)
    - spanned_drul_[LEFT]->relative_coordinate (common, X_AXIS)
    ;

  Real t = paper_l ()->get_var ("stafflinethickness");
  int l = line_count ();
  
  Real height = (l-1) * staff_space () /2;
  Molecule  m;
  for (int i=0; i < l; i++)
    {
      Molecule a =
	lookup_l ()->filledbox (Box (Interval (0,width),
				     Interval (-t/2, t/2)));

      a.translate_axis (height - i * staff_space (), Y_AXIS);
      m.add_molecule (a);
    }

  return m;
}

int
Staff_symbol::steps_i() const
{
  return line_count () * 2;
}

int
Staff_symbol::line_count () const
{
  return gh_scm2int (get_elt_property ("line-count"));
}

Real
Staff_symbol::staff_space ()const
{
  return gh_scm2double (get_elt_property ("staff-space"));
}
