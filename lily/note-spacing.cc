/*   
note-spacing.cc --  implement 

source file of the GNU LilyPond music typesetter

(c) 2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#include "note-spacing.hh"
#include "grob.hh"

bool
Note_spacing::has_interface (Grob* g)
{
  return g && g->has_interface (ly_symbol2scm ("note-spacing-interface"));
}
