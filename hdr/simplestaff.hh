/*
  simplestaff.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SIMPLESTAFF_HH
#define SIMPLESTAFF_HH

#include "key.hh"
#include "stcol.hh"
#include "staff.hh"
#include "swalker.hh"

/*
   mega-stupido staffs and cols: they do notes one at each moment.   
   */

struct Simple_staff;
struct Note_info {
    Rhythmic_req *rq;
    Array<Script_req*> scripts;
    Note_info();
    Note_info(Rhythmic_req*);
};

/// column of Simple_staff: store one request
struct Simple_column : Staff_column {

    Array<Note_info> notes;
    Array<Slur_req *> slurs;
    Stem_req *stem_;
    Moment stem_requester_len;
    Beam_req *beam_;
    Simple_staff* staff_;
    Text_req *text_;
    
    /****************/
    Slur_req  * find_slur(Voice *);
    void typeset_item(Item *, int=1);
    void typeset_item_directional(Item *, int dir, int=1);

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
    virtual Stem *get_stem(Stem_req *rq, Moment)=0;
    virtual Notehead *get_notehead(Note_req *rq, int b)=0;
    virtual Rest *get_rest(Rest_req *rq);
    virtual void set_output(PScore *);
    virtual Local_key_item* get_local_key_item();

    void process_commands( PCursor<Command*> &where);
    virtual void walk();

    Simple_staff();
};

#endif // SIMPLESTAFF_HH




