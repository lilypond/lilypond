/*
  voice-group-regs.hh -- declare Voice_group_registers

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEGROUPREGS_HH
#define VOICEGROUPREGS_HH

#include "register-group.hh"

struct Voice_group_registers  : Register_group_register {
    String group_id_str_;
    Array<Voice_registers*> voice_regs_l_;
    Moment termination_mom_;
    
    /* *************** */
    
    NAME_MEMBERS(Voice_group_registers);
    static bool static_acceptable_request_b(Request*);
    virtual void terminate_register(Request_register*);
    virtual void do_print() const;
    virtual void post_move_processing();
    virtual void add(Request_register*);
    Voice_group_registers(String id);
    virtual bool try_request(Request*);
};
#endif // VOICEGROUPREGS_HH
