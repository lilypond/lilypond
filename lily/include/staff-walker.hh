/*
  staff-walker.hh -- declare Staff_walker
  
  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFWALKER_HH
#define STAFFWALKER_HH

#include "proto.hh"
#include "time-description.hh"
#include "pcursor.hh"

/**
  manage run-time info when walking staffcolumns such as: key,
  meter, pending beams & slurs
  */
struct Staff_walker : public PCursor<Staff_column*> {
    Staff * staff_l_;
    PScore * pscore_l_;
    Score_walker *score_walk_l_;
    Time_description time_;
    Rhythmic_grouping *default_grouping;
    
    /* *************** */

    Moment when() const;    
    virtual ~Staff_walker();
    Staff_walker(Staff*, PScore*);
    void process() ;

    void operator++(int);
    void allow_break();

protected:
    /// every time before ++ is called
    virtual void do_pre_move(){}
    /// every time after ++ is called
    virtual void do_post_move(){}
    virtual void process_requests()=0;
private:
    void process_timing_reqs();
    Staff_walker(Staff_walker const&);
};

#endif // STAFFWALKER_HH

