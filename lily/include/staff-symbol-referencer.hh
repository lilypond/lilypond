/*   
  staff-sym-referencer.hh -- declare staff_symbol_referencer
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef STAFF_SYMBOL_REFERENCER_HH
#define STAFF_SYMBOL_REFERENCER_HH

#include "score-element.hh"

/**
   A notation object that needs access to variables of the staff (no
   lines, leading).
   
 */
class Staff_symbol_referencer : public virtual Score_element
{
  Real position_f_;
  
protected:
  Staff_symbol * staff_symbol_l_;

public:
  Staff_symbol_referencer ();
  void set_position (Real);
  
  void set_staff_symbol (Staff_symbol*);
  /**
     Leading are the lead strips between the sticks (lines) of
     typeface. ie. leading is vertical space.

   */
  Real staff_line_leading_f () const;
  Staff_symbol * staff_symbol_l () const;
  int lines_i () const;
  virtual void do_substitute_element_pointer (Score_element*,Score_element*);
  virtual void do_pre_processing ();
  virtual Real position_f () const;

};

#endif /* STAFF_SYMBOL_REFERENCER_HH */

