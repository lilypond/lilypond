/*
  local-key-reg.hh -- declare Local_key_register

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LOCALKEYREG_HH
#define LOCALKEYREG_HH

#include "register.hh"
#include "key.hh"
#include "parray.hh"

struct Local_key_register : Request_register {
    Key local_key_;
    Key const *key_C_;
    Array<Note_req* > mel_l_arr_;
    Array<Item* > support_l_arr_;
    Link_array<Item  > forced_l_arr_;
    Link_array<Item > tied_l_arr_;
    /* *************** */
    virtual void process_requests();
    virtual void acknowledge_element(Score_elem_info);
    virtual void pre_move_processing();
    Local_key_register();
    NAME_MEMBERS(Local_key_register);
};

#endif // LOCALKEYREG_HH
