//
//  lyricwalker.hh -- declare Lyric_walker
//
//  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.com>
//

#ifndef LYRICWALKER_HH
#define LYRICWALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "staffwalker.hh"
#include "lyriccolumn.hh"

/// a simple walker which collects words, and then print them, first on top
struct Lyric_walker: Staff_walker {
    Array<Lyric_item*> litem_l_array_;

    /* *************** */
    virtual void process_requests();
    
    Lyric_walker(Lyric_staff* lstaff_l);
    Lyric_column* lcol_l();
    Lyric_staff* lstaff_l();
};


#endif // LYRICWALKER_HH


