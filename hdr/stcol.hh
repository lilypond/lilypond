/*
  stcol.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STCOL_HH
#define STCOL_HH
#include "proto.hh"
#include "vray.hh"

/// store simultaneous requests
struct Staff_column {
    /// indirection
    Score_column *score_column;

    /// fields to collect data vertically.
    svec<Voice_element *> v_elts;

    /// idem
    Staff_commands_at *s_commands;

    Moment *moment_;
    
    /****************/
    
    Staff_column(Score_column*s); 
    bool mus() const;
    Real when() const;
    void add(Voice_element*ve);

    /****************************************************************
      VIRTUAL
    ****************************************************************/

    virtual void process_requests()=0;

    virtual ~Staff_column();
};


#endif // STCOL_HH

