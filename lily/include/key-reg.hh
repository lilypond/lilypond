/*
  key-reg.hh -- declare Key_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef KEYREG_HH
#define KEYREG_HH

#include "register.hh"
#include "key.hh"

struct Key_register : Request_register {
    Key key_;
    Key_change_req * keyreq_l_;
    Key_item * kit_p_;
    Array<int> accidental_idx_arr_;
    bool default_key_b_;
    
    virtual bool try_request(Request *req_l);
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    virtual void acknowledge_element(Staff_elem_info);
    Key_register();
    NAME_MEMBERS(Key_register);
private:
    
    void read_req(Key_change_req * r);
};

#endif // KEYREG_HH
