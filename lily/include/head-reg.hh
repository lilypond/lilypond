/*
  headreg.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADREG_HH
#define HEADREG_HH
#include "register.hh"

struct Note_head_register : Request_register {
    Note_head* note_p_;
    Rhythmic_req * note_req_l_;
    
    /* *************** */
    Note_head_register();
    virtual bool do_try_request(Request *req_l) ;
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
    NAME_MEMBERS();
};


#endif // HEADREG_HH
