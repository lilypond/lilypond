/*
  staff-regs.hh -- declare Staff_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef STAFF_REGS_HH
#define STAFF_REGS_HH

#include "register-group.hh"

/**
  Registers which manage a Staff (one 5-line linestaff)
 */
class Staff_registers : public Register_group_register {
    int c0_position_i_;    
    Input_register const *ireg_C_;
    int base_position_i_;
    Array<Voice_group_registers*> group_l_arr_;
 
public:
    
    /* *************** */
    NAME_MEMBERS(Staff_registers);
    void change_group(Group_change_req * greq_l,
		      Voice_registers *voice_regs_l,
		      Voice_group_registers * old_group);
    Voice_group_registers * get_group(String id);
    void terminate_register(Request_register * reg);
    virtual bool try_request(Request * r);
    virtual Staff_info get_staff_info();
    Staff_registers(Input_register const*);
    virtual bool acceptable_request_b(Request*) const ;
};

#endif // STAFF_REGS_HH
