/*
  staff-side.hh -- declare Staff_side

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH

#include "staff-elem.hh"

/// A symbol which sits along  the staff
class Staff_side  {
    Array<Staff_elem*> support_l_arr_;

    Staff_symbol * staff_sym_l_;
    Staff_elem * elem_l_;
    Interval support_height()const;
public:
       /**
      Vertical dir of symbol relative to staff. -1 = below staff?
      */
    int dir_i_;
    
    /// follow the support inside the staff?
    bool inside_staff_b_;

    void set_staffsym(Staff_symbol*);
  
    Staff_side(Staff_elem*);
    void add_support(Staff_elem*);
    
protected:
    int get_position_i()const;
    int get_position_i(Interval)const;
};
#endif // STAFF_SIDE_HH
