/*   
staff-spacing.cc --  implement 

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "staff-spacing.hh"
#include "grob.hh"

bool
Staff_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("staff-spacing-interface"));
}

