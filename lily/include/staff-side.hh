/*
  staff-side.hh -- declare Staff_side

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_SIDE_HH
#define STAFF_SIDE_HH

#include "score-elem.hh"
#include "interval.hh"

/** A symbol which sits along a staff

  Should override translate_y() ?
  */
class Staff_side : virtual Score_elem {
    Link_array<Score_elem> support_l_arr_;
    int staff_size_i_;
    Interval support_height()const;
    Staff_symbol* staff_sym_l_;
    int get_position_i()const;

    void read_staff_sym();
public:

    /**
      Vertical dir of symbol relative to staff. -1 = below staff?
      */
    int dir_i_;
    Interval sym_int_;
    
    /// follow the support inside the staff?
    bool inside_staff_b_;

    int pos_i_;

    void set_staffsym(Staff_symbol *  );
  
    Staff_side();
    void add_support(Score_elem*);
    DECLARE_MY_RUNTIME_TYPEINFO;
    
protected:
    virtual Interval symbol_height() const;
    virtual void do_substitute_dependency(Score_elem *, Score_elem*);
    virtual void do_post_processing();
};
#endif // STAFF_SIDE_HH
