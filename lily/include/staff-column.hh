/*
  staff-column.hh -- declare Staff_column

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFCOLUMN_HH
#define STAFFCOLUMN_HH

#include "lily-proto.hh"
#include "varray.hh"
#include "moment.hh"

/// store simultaneous requests
class Staff_column {

    Staff_column(Staff_column const&);

public:
    Array<Request*> creationreq_l_arr_;
    Array<Request*> musicalreq_l_arr_;
    Array<Request*> commandreq_l_arr_;
    Staff * staff_l_;
    Request_column * req_col_l_;
    /// fields to collect timing data vertically.
    Array<Timing_req*> timing_req_l_arr_;

    /* *************** */

    Staff_column();
    Score_column* command_column_l();
    Score_column* musical_column_l();
    Moment when() const;
    void set_req_col(Request_column *c1);
    void add_reqs (Array<Request*> req_l_arr);
    void OK() const;
    ~Staff_column();
    void typeset_breakable_items(Array<Item *> &pre_p_arr,
				 Array<Item *> &nobreak_p_arr,
				 Array<Item *> &post_p_arr);
    void typeset_musical_item(Item *i);
    void setup_one_request(Request*);
protected:
};



#endif // STAFFCOLUMN_HH

