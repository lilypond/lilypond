/*
  dots.cc -- implement Dots

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "dots.hh"

#include "item.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "directional-element-interface.hh"

MAKE_SCHEME_CALLBACK (Dots, print, 1);
SCM
Dots::print (SCM d)
{
  Grob *sc = unsmob_grob (d);
  Stencil mol;

  SCM c = sc->get_property ("dot-count");

  if (scm_is_number (c))
    {
      Stencil d = Font_interface::get_default_font (sc)->find_by_name (std::string ("dots.dot"));
      Real dw = d.extent (X_AXIS).length ();

      /*
	we need to add a real blank box, to assure that
	side-positioning doth not cancel the left-most padding.  */

      /*
	TODO: this should  be handled by side-position padding.
      */
      mol = Lookup::blank (Box (Interval (0, 0),
				Interval (0, 0)));

      for (int i = scm_to_int (c); i--;)
	{
	  d.translate_axis (2 * dw, X_AXIS);
	  mol.add_at_edge (X_AXIS, RIGHT, d, dw, 0);
	}
    }
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Dots, "dots-interface",
	       "The dots to go with a notehead or rest."
	       "@code{direction} sets the preferred direction to move in case of staff "
	       "line collisions.",

	       /* properties */
	       "direction "
	       "dot-count");

