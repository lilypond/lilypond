/*
  meter-reg.hh -- declare  Meter_register

  source file of the GNU LilyPond music typesetter

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
    virtual void process_requests();
    virtual void pre_move_processing();
    virtual void post_move_processing();
    Meter_register();
    NAME_MEMBERS(Meter_register);
};
#endif // METERREG_HH
