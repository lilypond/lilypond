/*
  staff-regs.hh -- declare Staff_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_REGS_HH
#define STAFF_REGS_HH

#include "register-group.hh"

/**
  Registers which manage a Staff (one 5-line linestaff)
  
 */
class Staff_registers : public Register_group_register {
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
    Staff_registers();
};

#endif // STAFF_REGS_HH
