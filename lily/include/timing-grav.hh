/*
  timing-grav.hh -- declare Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef TIMING_GRAV_HH
#define TIMING_GRAV_HH

#include "engraver.hh"
#include "time-description.hh"
#include "grouping.hh"
#include "parray.hh"

/**
  Do time bookkeeping
 */
class Timing_engraver : public Engraver
{   
    Time_description time_;
    Rhythmic_grouping  default_grouping_;
    
    Link_array<Timing_req> timing_req_l_arr_;
 
    virtual void fill_staff_info(Staff_info&);
    virtual bool do_try_request(Request *req_l);
    virtual void do_process_requests();
    virtual void do_pre_move_processing();
    virtual void do_creation_processing();
    virtual void do_post_move_processing();
    Timing_engraver();
    DECLARE_MY_RUNTIME_TYPEINFO;
}

#endif // TIMING_GRAV_HH
