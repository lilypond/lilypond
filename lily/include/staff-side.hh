/*
  staff-side.hh -- declare Staff_side

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH

#include "score-elem.hh"

/// A symbol which sits along  the staff
class Staff_side : virtual Score_elem {
    Link_array<Score_elem> support_l_arr_;
    int staff_size_i_;
    Interval support_height()const;
    Staff_symbol* staff_sym_l_;

    void read_staff_sym();
public:

    /**
      Vertical dir of symbol relative to staff. -1 = below staff?
      */
    int dir_i_;
    
    /// follow the support inside the staff?
    bool inside_staff_b_;

    void set_staffsym(Staff_symbol *  );
  
    Staff_side();
    void add_support(Score_elem*);
    NAME_MEMBERS();
    
protected:
    virtual void do_substitute_dependency(Score_elem *, Score_elem*);
    int get_position_i()const;
    int get_position_i(Interval)const;
};
#endif // STAFF_SIDE_HH
