/*
  headreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADREG_HH
#define HEADREG_HH
#include "register.hh"

struct Notehead_register : Request_register {
    Item* note_p_;
    int dir_i_;
    /* *************** */
    Notehead_register(Complex_walker*);
    virtual bool try_request(Request *req_l) ;
    virtual void process_request();
    virtual void do_pre_move_process();
    void set_dir(int);
};


#endif // HEADREG_HH
