/*
  meterreg.hh -- declare  Meter_register

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef METERREG_HH
#define METERREG_HH
#include "register.hh"

/**
  generate meters. 
  */
class Meter_register : public Request_register {
public:
    Meter_change_req * meter_req_l_;
    Meter * meter_p_;
 
    virtual bool try_request(Request *req_l);
    virtual void process_request();
    virtual void do_pre_move_process();
    virtual void do_post_move_process();
    Meter_register(Complex_walker*);
};
#endif // METERREG_HH
