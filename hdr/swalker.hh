
/*
  swalker.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SWALKER_HH
#define SWALKER_HH

#include "staff.hh"

struct Staff_walker : public PCursor<Staff_column*> {
    Staff * staff_;
    PScore * pscore_;

    int break_status;
    
    /****************/

    int priority() const;		// Command
    Moment when() const;    
    virtual ~Staff_walker();
    Staff_walker(Staff*, PScore*);
    void process() ;
    void process_command(Command *);
    void operator++(int);
    /// every time ++ is called
    virtual void reset()=0;
    virtual void process_requests()=0;
    virtual void do_TYPESET_command(Command*)=0;
    virtual void do_INTERPRET_command(Command*)=0 ;
};
/**
  manage run-time info when walking staffcolumns such as: key,
  meter, pending beams & slurs
  */

#endif // SWALKER_HH

