/*
  headreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADREG_HH
#define HEADREG_HH
#include "register.hh"

struct Notehead_register : Request_register {
    Item* note_p_;
    Rhythmic_req * note_req_l_;
    int dir_i_;
    
    /* *************** */
    Notehead_register();
    virtual bool try_request(Request *req_l) ;
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    void set_feature(Features);
    NAME_MEMBERS(Notehead_register);
};


#endif // HEADREG_HH
