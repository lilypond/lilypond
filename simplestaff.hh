/*
  simplestaff.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SIMPLESTAFF_HH
#define SIMPLESTAFF_HH

#include "stcol.hh"
#include "staff.hh"
/*
   mega-stupido staffs and cols: they do notes one at each moment.   
   */

struct Simple_staff;

/// column of Simple_staff: store one request
struct Simple_column : Staff_column {

    Request *the_note;
    Simple_staff* staff_;

    /****************/
    
    virtual void typeset_req(Request *rq)=0;
    virtual void typeset_command(Command *, int brs)=0;
    virtual void typeset_item(Item *, int=1);

    Item *create_command_item(Command *com);
    Item *create_req_item(Request *rq);
    void take_request(Request *rq);
    
    virtual void process_commands( );
    virtual void process_requests();

    Simple_column(Score_column*s,Simple_staff*rs);
};


/// Simple  staff: one note at a time
struct Simple_staff : Staff {
    /// indirection to the PStaff.
    PStaff *theline;

    /****************/
    virtual void set_output(PScore *);

    void process_commands( PCursor<Command*> &where);
    void grant_requests();
//    virtual Staff*clone()const;

    Simple_staff();
};

#endif // SIMPLESTAFF_HH



