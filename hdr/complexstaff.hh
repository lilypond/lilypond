/*
  complexstaff.hh -- part of LilyPond

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#ifndef COMPLEXSTAF_HH
#define COMPLEXSTAF_HH


#include "key.hh"
#include "stcol.hh"
#include "staff.hh"
#include "staffwalker.hh"

/// column of Complex_staff: store one request
struct Complex_column : Staff_column {

    Array<Request*> first_l_arr_;
    Array<Request*> second_l_arr_;

    Complex_staff* staff_l_;
    
    /****************/

    Slur_req *find_slur(Voice *);

    void typeset_item(Item *, int=1);
    void typeset_item_directional(Item *, int dir, int=1);
    Molecule *create_command_mol(Command *com);

    void take_request(Request *rq);   
    virtual void setup_requests();

    Complex_column(Score_column*s,Complex_staff*rs);
};


/// Complex  staff: one voicegroup  at a time
struct Complex_staff : Staff {
    /// indirection to the PStaff.
    PStaff *theline_l_;

    /****************/
    Staff_column*create_col(Score_column*);    
    virtual Item *get_TYPESET_item(Command*);
    virtual void set_output(PScore *);
    void process_commands( PCursor<Command*> &where);
    virtual void walk();

    Complex_staff();
};

#endif // COMPLEXSTAF_HH

