/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "dots.hh"

#include "item.hh"
#include "output-def.hh"
#include "font-interface.hh"
#include "lookup.hh"
#include "staff-symbol-referencer.hh"
#include "international.hh"

MAKE_SCHEME_CALLBACK (Dots, print, 1);
SCM
Dots::print (SCM d)
{
  Grob *sc = unsmob_grob (d);
  Stencil mol;

  SCM c = sc->get_property ("dot-count");

  if (scm_is_number (c))
    {
      SCM scm_style = sc->get_property ("style");
      string style ="";
      if (scm_is_symbol (scm_style))
	style = ly_symbol2string (scm_style);
      string idx =  "dots.dot" + style;
      Stencil d = Font_interface::get_default_font (sc)->find_by_name (idx);
      if (d.is_empty ())
	{
	  sc->warning (_f ("dot `%s' not found", idx.c_str ()));
	  return SCM_EOL;
	}
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
	  mol.add_at_edge (X_AXIS, RIGHT, d, dw);
	}
    }
  return mol.smobbed_copy ();
}

ADD_INTERFACE (Dots,
	       "The dots to go with a notehead or rest.  @code{direction}"
	       " sets the preferred direction to move in case of staff line"
	       " collisions.  @code{style} defaults to undefined, which is"
	       " normal 19th/20th century traditional style.  Set"
	       " @code{style} to @code{vaticana} for ancient type dots.",

	       /* properties */
	       "direction "
	       "dot-count "
	       "style "
	       );
