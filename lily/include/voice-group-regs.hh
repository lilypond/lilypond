/*
  voice-group-regs.hh -- declare Voice_group_registers

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef VOICEGROUPREGS_HH
#define VOICEGROUPREGS_HH

#include "register-group.hh"

/**
  A group of voices which share certain characteristics (such as beams. ).
 */
class Voice_group_registers  : public Register_group_register {
    Moment termination_mom_;
    int dir_i_;

protected:
    virtual void do_print() const;
    virtual Scalar get_feature(String);
    virtual void do_post_move_processing();
    virtual bool do_try_request(Request*);
public:
    
    
    NAME_MEMBERS();
    static bool static_acceptable_request_b(Request*);
    Voice_group_registers();
};
#endif // VOICEGROUPREGS_HH
