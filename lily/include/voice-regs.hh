/*
  voice-regs.hh -- declare Voice_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEREGS_HH
#define VOICEREGS_HH

#include "register-group.hh"

class Voice_registers : public Register_group_register {
    Input_register const *ireg_C_;
    bool terminate_b_;
public:
    Voice *voice_l_;
    /* *************** */

    Voice_registers(Voice*, Input_register const*);
    NAME_MEMBERS(Voice_registers);

protected:
    virtual void pre_move_processing();
    virtual bool acceptable_request_b(Request*);
    virtual void acknowledge_element(Staff_elem_info info);
    virtual bool try_request(Request*);
    virtual void do_print() const;
};


#endif // VOICEREGS_HH
