/*
  script-reg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPTREG_HH
#define SCRIPTREG_HH

#include "register.hh"


struct Script_register : Request_register {
    Script * script_p_;
    Script_req * script_req_l_;
    int dir_i_;
    /* *************** */
    void set_feature(Features dir_i_);
    Script_register();
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void pre_move_processing();
    virtual void post_move_processing();
    NAME_MEMBERS(Script_register);
};

#endif // SCRIPTREG_HH
