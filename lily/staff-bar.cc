/*   
  staff-bar.cc --  implement Staff_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "staff-bar.hh"

Real
Staff_bar::get_bar_size () const
{
  SCM size = get_elt_property ("bar-size");
  if (gh_number_p (size))
    return gh_scm2double (size);
  else
    return (lines_i () -1) * staff_line_leading_f ();
}

void
Staff_bar::do_pre_processing ()
{
  Bar::do_pre_processing ();
  Staff_symbol_referencer::do_pre_processing ();
}
