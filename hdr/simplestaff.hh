/*
  simplestaff.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SIMPLESTAFF_HH
#define SIMPLESTAFF_HH

#include "stcol.hh"
#include "staff.hh"
#include "swalker.hh"
/*
   mega-stupido staffs and cols: they do notes one at each moment.   
   */

struct Simple_staff;

/// column of Simple_staff: store one request
struct Simple_column : Staff_column {

    svec<Rhythmic_req *> notes;
    Stem_req *stem_;
    Beam_req *beam_;
    Simple_staff* staff_;

    /****************/

    virtual void typeset_item(Item *, int=1);

    Molecule *create_command_mol(Command *com);

    void take_request(Request *rq);   
    virtual void process_requests();

    Simple_column(Score_column*s,Simple_staff*rs);
};


/// Simple  staff: one voicegroup  at a time
struct Simple_staff : Staff {
    /// indirection to the PStaff.
    PStaff *theline;

    /****************/
    Staff_column*create_col(Score_column*);
    
    virtual Item *get_TYPESET_item(Command*);
    virtual Stem *get_stem(Stem_req *rq)=0;
    virtual Notehead *get_notehead(Note_req *rq)=0;
    virtual Rest *get_rest(Rest_req *rq);
    virtual void set_output(PScore *);

    void process_commands( PCursor<Command*> &where);
    virtual void walk();

    Simple_staff();
};

struct Simple_walker: Staff_walker {
    Stem *stem_;
    svec<Notehead *>noteheads;
    Beam *beam_;
    
    /****************/
    
    virtual void process_command(Command*);
    virtual void process_requests();
    Simple_walker(Simple_staff*);
    Simple_column *col();
    Simple_staff *staff();
};


#endif // SIMPLESTAFF_HH




