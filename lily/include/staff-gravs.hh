/*
  staff-gravs.hh -- declare Staff_engravers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_GRAVS_HH
#define STAFF_GRAVS_HH

#include "engraver-group.hh"

/**
  Engravers which manage a Staff (one 5-line linestaff)
  
 */
class Staff_engravers : public Engraver_group_engraver {
    Line_of_staff *staffline_p_;   
    Link_array<Score_elem> staff_elem_l_arr_;

    void group_staff_elems();
protected:
    virtual void do_pre_move_processing();
    virtual void do_creation_processing();
    virtual void do_removal_processing();
    virtual void typeset_element(Score_elem*);
    virtual void typeset_breakable_item( Item * it_p);

public:
    
    NAME_MEMBERS();
    Staff_engravers();
};

#endif // STAFF_GRAVS_HH
