/*
  voice-group-regs.hh -- declare Voice_group_registers

  source file of the LilyPond music typesetter

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
    Input_register const *ireg_C_;
    int dir_i_;

protected:
    virtual void do_print() const;
    virtual void set_feature(Feature);
    virtual Scalar get_feature(String);
    virtual void post_move_processing();
    virtual bool try_request(Request*);
public:
    void OK() const;
    virtual Request_register * get_register_p(Request_register  * reg_l);
    /// each group in a staff has an unique ID.
    String group_id_str_;

    /// The pointers are in the base class. This is just administration
    Array<Voice_registers*> voice_reg_l_arr_;
    /* *************** */
    
    NAME_MEMBERS(Voice_group_registers);
    static bool static_acceptable_request_b(Request*);
    virtual void add(Request_register*);
    Voice_group_registers(String id, Input_register const *);
};
#endif // VOICEGROUPREGS_HH
