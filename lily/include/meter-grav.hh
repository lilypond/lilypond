/*
  meter-grav.hh -- declare  Meter_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef METERGRAV_HH
#define METERGRAV_HH
#include "engraver.hh"
#include "time-description.hh"
#include "grouping.hh"

/**
  generate meters. 
  */
class Meter_engraver : public Engraver {
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
    Meter_engraver();
    DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // METERGRAV_HH
