//
// lyriccolumn.hh -- part of LilyPond
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>


#ifndef LYRICCOLUMN_HH
#define LYRICCOLUMN_HH

#include "key.hh"
#include "stcol.hh"
#include "staff.hh"
#include "staffwalker.hh"

struct Lyric_staff;

/// (winfo)
struct Word_info {
    Lyric_req* lreq_l_;
    Word_info();
    Word_info(Lyric_req* lreq_l);
};

/// (lcol)
struct Lyric_column : Staff_column {

    Array<Word_info> winfo_array_;
    Lyric_staff* lstaff_l_;
    
    void typeset_item(Item *, int=1);
//    void typeset_item_directional(Item *, int dir, int=1);

//    Molecule *create_command_mol(Command *com);

//    void take_request(Request *rq);   
    virtual void process_requests();

    Lyric_column(Score_column*s,Lyric_staff*rs);
};

#endif // LYRICSTAFF_HH




