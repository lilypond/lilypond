/* 
 staff-sym-referencer.hh -- declare staff_symbol_referencer
 
 source file of the GNU LilyPond music typesetter
 
  (c) 1999--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 
 */

#ifndef STAFF_SYMBOL_REFERENCER_HH
#define STAFF_SYMBOL_REFERENCER_HH

#include "grob.hh"


/**
 A notation object that needs access to variables of the staff (no
 lines, leading).
 
 */
class Staff_symbol_referencer 
{
public:
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  static void set_position (Grob*,Real);
  DECLARE_SCHEME_CALLBACK (callback, (SCM element, SCM axis));

  /**
     Leading are the lead strips between the sticks (lines) of
     typeface. ie. leading is vertical space.
  */
 
  static Real staff_space (Grob*);
  static Grob * staff_symbol_l (Grob*);
  static bool on_staffline (Grob*);
  static bool on_staffline (Grob*,int);
  static int line_count (Grob*);
  static Real position_f (Grob*);
  static Real staff_radius (Grob*);
};

int compare_position (Grob *const&,Grob *const&); 
#endif /* STAFF_SYMBOL_REFERENCER_HH */

