/*
  dots.cc -- implement Dots

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dots.hh"
#include "item.hh"
#include "stencil.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

MAKE_SCHEME_CALLBACK (Dots,print,1);
SCM  
Dots::print (SCM d)
{
  Grob *sc = unsmob_grob (d);
  Stencil mol;
  
  SCM c = sc->get_grob_property ("dot-count");

  if (gh_number_p (c))
    {
      Stencil d = Font_interface::get_default_font (sc)->find_by_name (String ("dots-dot"));
      Real dw = d.extent (X_AXIS).length ();
      

      /*
	we need to add a real blank box, to assure that
	side-positioning doth not cancel the left-most padding.  */

      /*
	TODO: this should  be handled by side-position padding.
       */
      mol = Lookup::blank (Box (Interval (0,0),
				Interval (0,0)));
  
      for (int i = gh_scm2int (c); i--;)
	{
	  d.translate_axis (2*dw,X_AXIS);
	  mol.add_at_edge (X_AXIS, RIGHT, d, dw, 0);
	}
    }
  return mol.smobbed_copy ();
}




ADD_INTERFACE (Dots, "dots-interface",
  "The dots to go with a notehead/rest.  A separate interface, since they "
" are a party in collision resolution. "
" #'direction is the Direction to handle staff-line collisions in.",
  "direction dot-count");


