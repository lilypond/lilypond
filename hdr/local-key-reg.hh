/*
  local-key-reg.hh -- declare Local_key_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LOCALKEYREG_HH
#define LOCALKEYREG_HH
#include "register.hh"
#include "key.hh"

struct Local_key_register : Request_register {
    Local_key local_key_;
    Local_key_item* key_item_p_;
    const Key *key_c_l_;
    /* *************** */
    virtual void process_requests();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void pre_move_processing();
    Local_key_register();
    NAME_MEMBERS(Local_key_register);
};

#endif // LOCALKEYREG_HH
