/*
  dots.cc -- implement Dots

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "item.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"


Real
Dots::quantised_position_callback (Score_element * me, Axis a)
{
  assert (a == Y_AXIS);
    
  SCM d= me->get_elt_property ("dot-count");
  if (gh_number_p (d) && gh_scm2int (d))
    {
      if (!Directional_element_interface::get (me))
	Directional_element_interface::set (me, UP);

      if (Staff_symbol_referencer::on_staffline (me))
	return Staff_symbol_referencer::staff_space (me) / 2.0 * Directional_element_interface::get (me);
    }

  return  0.0;
}


MAKE_SCHEME_CALLBACK(Dots,brew_molecule);
SCM  
Dots::brew_molecule (SCM d)
{
  Score_element *sc = unsmob_element (d);
  Molecule mol (sc->lookup_l ()->blank (Box (Interval (0,0),
					 Interval (0,0))));

  SCM c = sc->get_elt_property ("dot-count");
  if (gh_number_p (c))
    {
      Molecule d = sc->lookup_l ()->afm_find (String ("dots-dot"));

      Real dw = d.extent (X_AXIS).length ();
      d.translate_axis (-dw, X_AXIS);


      for (int i = gh_scm2int (c); i--; )
	{
	  d.translate_axis (2*dw,X_AXIS);
	  mol.add_molecule (d);
	}
    }
  return mol.create_scheme ();
}



