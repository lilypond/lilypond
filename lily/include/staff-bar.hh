/*   
  staff-bar.hh -- declare Staff_bar
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_BAR_HH
#define STAFF_BAR_HH

#include "bar.hh"
#include "staff-symbol-referencer.hh"

/**
   A bar that is on a staff.
   Ugh. Entita non multiplicandum  ... 
 */
class Staff_bar : public Bar, public Staff_symbol_referencer
{
public:
  virtual void do_pre_processing ();
  VIRTUAL_COPY_CONS(Score_element);
  virtual Real get_bar_size () const;
};

#endif /* STAFF_BAR_HH */

