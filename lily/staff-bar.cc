/*   
  staff-bar.cc --  implement Staff_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-bar.hh"

Real
Staff_bar::get_bar_size () const
{
  SCM size_sym = get_elt_property (bar_size_scm_sym);
  if (size_sym != SCM_BOOL_F)
    return gh_scm2double (SCM_CDR(size_sym));
  else
    return (lines_i () -1) * staff_line_leading_f ();
}
