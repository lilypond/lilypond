/* 
 staff-sym-referencer.hh -- declare staff_symbol_referencer
 
 source file of the GNU LilyPond music typesetter
 
 (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 
 */

#ifndef STAFF_SYMBOL_REFERENCER_HH
#define STAFF_SYMBOL_REFERENCER_HH

#include "score-element.hh"


/**
 A notation object that needs access to variables of the staff (no
 lines, leading).
 
 */
class Staff_symbol_referencer 
{
public:
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);
  static void set_position (Score_element*,Real);
  static Real callback (Score_element *, Axis a);

  /**
     Leading are the lead strips between the sticks (lines) of
     typeface. ie. leading is vertical space.
  */
 
  static Real staff_space (Score_element*);
  static Score_element * staff_symbol_l (Score_element*);
  static int line_count (Score_element*);
  static Real position_f (Score_element*);
};

int compare_position (Score_element *const&,Score_element *const&); 
#endif /* STAFF_SYMBOL_REFERENCER_HH */

