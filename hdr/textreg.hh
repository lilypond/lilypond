/*
  textreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TEXTREG_HH
#define TEXTREG_HH
#include "register.hh"

struct Text_register : Request_register{
    Text_item * text_p_;
    int dir_i_;
    /* *************** */
    void set_dir(int dir_i);
    Text_register(Complex_walker*);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void do_pre_move_process();
};

#endif // TEXTREG_HH
