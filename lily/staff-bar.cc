/*   
  staff-bar.cc --  implement Staff_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-bar.hh"
#include "staff-symbol-referencer.hh"

Real
Staff_bar::get_bar_size () const
{
  SCM size = get_elt_property ("bar-size");
  if (gh_number_p (size))
    return gh_scm2double (size);
  else
    {
      Staff_symbol_referencer_interface si (this);
      return (si.lines_i () -1) * si.staff_space ();
    }
}

