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



MAKE_SCHEME_SCORE_ELEMENT_CALLBACKS(Staff_symbol)
Molecule 
Staff_symbol::do_brew_molecule () const
{
  Score_element * common
    = get_bound (LEFT)->common_refpoint (get_bound (RIGHT), X_AXIS);
  
  Real width =
    // right_shift     - left_shift
    + get_bound (RIGHT)->relative_coordinate (common , X_AXIS)
    - get_bound (LEFT)->relative_coordinate (common, X_AXIS)
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
  return gh_scm2double (get_elt_property ("staff-space")) *
    paper_l ()->get_var ("staffspace");
}

Staff_symbol::Staff_symbol( SCM s)
  : Spanner (s)
{
}
