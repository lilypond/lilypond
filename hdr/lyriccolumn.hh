//
// lyriccolumn.hh -- declare Lyric_column
//
// copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>


#ifndef LYRIC_COLUMN_HH
#define LYRIC_COLUMN_HH

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

#endif // LYRIC_COLUMN_HH




