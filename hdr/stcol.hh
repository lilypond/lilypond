/*
  stcol.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STCOL_HH
#define STCOL_HH
#include "proto.hh"
#include "varray.hh"
#include "moment.hh"

/// store simultaneous requests
struct Staff_column {

    Score_column *score_column_l_;

    /// fields to collect data vertically.
    Array<Voice_element *> v_elts;

    /// idem
    Staff_commands_at *staff_commands_p_;

    Time_description *tdescription_;
    
    /****************/
    
    Staff_column(Score_column*s); 
    bool mus() const;
    Moment when() const;
    void add(Voice_element*ve);
    void OK() const;
    /****************************************************************
      VIRTUAL
    ****************************************************************/

    virtual void process_requests()=0;

    virtual ~Staff_column();
private:
    Staff_column(Staff_column const&){}
};


#endif // STCOL_HH

