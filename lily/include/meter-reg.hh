/*
  meter-reg.hh -- declare  Meter_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef METERREG_HH
#define METERREG_HH
#include "register.hh"
#include "time-description.hh"
#include "grouping.hh"

/**
  generate meters. 
  */
class Meter_register : public Request_register {
public:
    Time_description time_;
    Rhythmic_grouping  default_grouping_;
    
    Meter_change_req * meter_req_l_;
    Meter * meter_p_;
 
    virtual void fill_staff_info(Staff_info&);
    virtual bool do_try_request(Request *req_l);
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_creation_processing();
    virtual void do_post_move_processing();
    Meter_register();
    NAME_MEMBERS();
};
#endif // METERREG_HH
