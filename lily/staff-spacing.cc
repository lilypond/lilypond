/*   
staff-spacing.cc --  implement Staff_spacing

source file of the GNU LilyPond music typesetter

(c) 2001--2002  Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "staff-spacing.hh"
#include "grob.hh"

bool
Staff_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("staff-spacing-interface"));
}


/*
  
  TODO: move computation of clef/key-sig/whatever to first-note
  distance here.

*/
