/*
  localkeyreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef LOCALKEYREG_HH
#define LOCALKEYREG_HH
#include "register.hh"
#include "key.hh"

struct Local_key_register : Request_register {
    Local_key local_key_;
    Local_key_item* key_item_p_;

    /* *************** */
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
    Local_key_register(Complex_walker*);
};

#endif // LOCALKEYREG_HH
