/*
  complexcolumn.hh -- declare Complex_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COMPLEXCOLUMN_HH
#define COMPLEXCOLUMN_HH

#include "stcol.hh"

/// column of Complex_staff: store one request
struct Complex_column : Staff_column {

    Array<Request*> first_l_arr_;
    Array<Request*> second_l_arr_;

    Complex_staff* staff_l_;
    
    /* *************** */

    Slur_req *find_slur(Voice *);

    void typeset_musical_item(Item *);
    void typeset_breakable_items(Array<Item *> &pre_p_arr,
				 Array<Item *> &nobreak_p_arr,
				 Array<Item *> &post_p_arr);
    virtual void setup_one_request(Request*);

    Complex_column(Complex_staff*rs);
};

#endif // COMPLEXCOLUMN_HH
