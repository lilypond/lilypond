/*
  stcol.hh -- declare Staff_column

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STCOL_HH
#define STCOL_HH
#include "proto.hh"
#include "varray.hh"
#include "moment.hh"

/// store simultaneous requests
class Staff_column {
    Staff_column(Staff_column const&);
public:
    /// fields to collect timing data vertically.
    Array<Timing_req*> timing_req_l_arr_;
    Score_column *musical_column_l_, *command_column_l_;

    /* *************** */
    
    Staff_column();

    Moment when() const;
    void set_cols(Score_column *c1, Score_column *c2);
    void add(Voice_element*ve);
    void OK() const;
    virtual ~Staff_column();


protected:
    virtual void setup_one_request(Request*)=0;

};


#endif // STCOL_HH

