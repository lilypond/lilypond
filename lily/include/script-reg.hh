/*
  script-reg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPTREG_HH
#define SCRIPTREG_HH

#include "register.hh"


class Script_register : public Request_register {
    Array<Script *> script_p_arr_;
    Array<Script_req *> script_req_l_arr_;

    /* *************** */
    bool acceptable_elem_b(Staff_elem*);
public:
    NAME_MEMBERS(Script_register);
    Script_register();
protected:
    virtual    void set_feature(Features dir_i_);
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void pre_move_processing();
    virtual void post_move_processing();

};

#endif // SCRIPTREG_HH
