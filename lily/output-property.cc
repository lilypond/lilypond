/*   
  output-property.cc --  implement Output_property
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "output-property.hh"
#include "lily-guile.hh"

Output_property::Output_property(SCM pred, SCM sym, SCM val)
{
  pred_sym_val_list_ = gh_list (pred, sym, val, SCM_UNDEFINED);
}

