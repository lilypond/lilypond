/*
  scriptreg.hh -- part of LilyPond

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SCRIPTREG_HH
#define SCRIPTREG_HH

#include "register.hh"


struct   Script_register : Request_register {
    Script * script_p_;
    int dir_i_;
    /* *************** */
    void set_dir(int dir_i_);
    Script_register(Complex_walker*);
    virtual bool try_request(Request*);
    virtual void process_request();
    virtual void acknowledge_element(Staff_elem_info);
    virtual void do_pre_move_process();
};

#endif // SCRIPTREG_HH
