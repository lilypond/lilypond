/*
  complexcolumn.hh -- declare Complex_column

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COMPLEXCOLUMN_HH
#define COMPLEXCOLUMN_HH

/// column of Complex_staff: store one request
struct Complex_column : Staff_column {

    Array<Request*> first_l_arr_;
    Array<Request*> second_l_arr_;

    Complex_staff* staff_l_;
    
    /* *************** */

    Slur_req *find_slur(Voice *);

    void typeset_item(Item *, int=1);
    void typeset_item_directional(Item *, int dir, int=1);
    Molecule *create_command_mol(Command *com);

    void take_request(Request *rq);   
    virtual void setup_requests();

    Complex_column(Score_column*s,Complex_staff*rs);
};

#endif // COMPLEXCOLUMN_HH
