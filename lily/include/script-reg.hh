/*
  script-reg.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPTREG_HH
#define SCRIPTREG_HH

#include "register.hh"


class Script_register : public Request_register {
    Array<Script *> script_p_arr_;
    Array<Script_req *> script_req_l_arr_;

    /* *************** */
    bool acceptable_elem_b(Score_elem*);
public:
    NAME_MEMBERS();
    Script_register();
protected:
    virtual bool do_try_request(Request*);
    virtual void do_process_requests();
    virtual void acknowledge_element(Score_elem_info);
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();

};

#endif // SCRIPTREG_HH
