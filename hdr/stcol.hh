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
    Array<Request*> musicalreq_l_arr_;
    Array<Request*> commandreq_l_arr_;
    Staff * staff_l_;

    /// fields to collect timing data vertically.
    Array<Timing_req*> timing_req_l_arr_;
    Score_column *musical_column_l_, *command_column_l_;

    /* *************** */
    
    Staff_column();

    Moment when() const;
    void set_cols(Score_column *c1, Score_column *c2);
    void add(Voice_element*ve);
    void OK() const;
    ~Staff_column();
    void typeset_breakable_items(Array<Item *> &pre_p_arr,
				 Array<Item *> &nobreak_p_arr,
				 Array<Item *> &post_p_arr);
    void typeset_musical_item(Item *i);
protected:
     void setup_one_request(Request*);
};


#endif // STCOL_HH

