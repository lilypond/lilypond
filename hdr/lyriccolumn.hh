//
// lyriccolumn.hh -- declare Lyric_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>


#ifndef LYRICCOLUMN_HH
#define LYRICCOLUMN_HH

#include "key.hh"
#include "stcol.hh"
#include "staff.hh"

/// (lcol)
struct Lyric_column : Staff_column {

    Array<Lyric_req*> lreq_l_array_;
    Lyric_staff* lstaff_l_;
    
    void typeset_item(Item *);
    virtual void setup_one_request(Request*);

    Lyric_column(Lyric_staff*rs);
};

#endif // LYRICSTAFF_HH




