/*
  slur-reg.hh -- declare Slur_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SLURREG_HH
#define SLURREG_HH

#include "register.hh"

struct Slur_register : Request_register {
    Array<Slur_req*> requests_arr_;
    Array<Slur_req*> new_slur_req_l_arr_;
    Array<Slur *> slur_l_stack_;
    Array<Slur*> end_slur_l_arr_;

    int dir_i_;
    
    /* *************** */
    ~Slur_register();
    Slur_register();
    virtual bool try_request(Request*);
    virtual void process_requests();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual void set_feature(Features);
    NAME_MEMBERS(Slur_register);
};

#endif // SLURREG_HH
