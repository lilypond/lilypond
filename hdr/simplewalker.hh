/*
  simplewalker.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SIMPLEWALKER_HH
#define SIMPLEWALKER_HH

#include "proto.hh"
#include "grouping.hh"

struct Simple_walker: Staff_walker {
    Stem *stem_;
    Array<Notehead *>noteheads;
    Local_key local_key_;
    Key key_;
    Array<int> *oldkey_undo;
    Array<int> typesetkey;
    Beam *beam_;
    Local_key_item *local_key_item_;
    bool wantkey;		// urgh
    int processed_bar_priority;
    bool processed_key;
    bool processed_clef;
    Clef clef_;
    Rhythmic_grouping default_grouping;
    Rhythmic_grouping *current_grouping;
    Array<Slur_req*> pending_slur_reqs;
    Array<Slur*>  pending_slurs;

    /****************/

    virtual void do_TYPESET_command(Command*);
    virtual void do_INTERPRET_command(Command*);
    virtual void process_requests();
    virtual void reset();
    
    void do_note(Note_info);
    Simple_walker(Simple_staff*);
    Simple_column *col();
    Simple_staff *staff();

    void do_local_key(Note_req*, Notehead*);
    int find_slur(const Voice*v);
};


#endif // SIMPLEWALKER_HH


