/*   
  staff-sym-referencer.hh -- declare Staff_sym_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_SYM_REFERENCER_HH
#define STAFF_SYM_REFERENCER_HH

#include "score-element.hh"

/**
   A notation object that needs access to variables of the staff (no
   lines, leading).
   
 */
class Staff_symbol_referencer : public virtual Score_element
{
protected:
  Staff_symbol * staff_sym_l_;

public:
  Staff_symbol_referencer ();
  void set_staff_symbol (Staff_symbol*);
  /**
     Leading are the lead strips between the sticks (lines) of
     typeface. ie. leading is vertical space.

   */
  Real staff_line_leading_f () const;
  Staff_symbol * staff_symbol_l () const;
  int lines_i () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);

  virtual Real position_f () const;
};

#endif /* STAFF_SYM_REFERENCER_HH */

