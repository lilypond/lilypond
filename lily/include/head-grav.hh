/*
  head-grav.hh -- part of GNU LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADGRAV_HH
#define HEADGRAV_HH
#include "engraver.hh"

struct Note_head_engraver : Engraver {
    Note_head* note_p_;
    Rhythmic_req * note_req_l_;
    
    /* *************** */
    Note_head_engraver();
    virtual bool do_try_request(Request *req_l) ;
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_post_move_processing();
    NAME_MEMBERS();
};


#endif // HEADGRAV_HH
