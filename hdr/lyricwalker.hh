/*
  simplewalker.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys, Jan Nieuwenhuizen <jan@digicash.nl>
*/

#ifndef LYRICWALKER_HH
#define LYRICWALKER_HH

#include "proto.hh"
#include "grouping.hh"
#include "staffwalker.hh"
#include "lyriccolumn.hh"

struct Lyric_item; // put into proto
struct Lyric_walker: Staff_walker {
    Array<Lyric_item*> litem_l_array_;

    /* *************** */
    
    virtual void do_TYPESET_command(Command*);
    virtual void do_INTERPRET_command(Command*);
    virtual void process_requests();
    virtual void reset();
    
    void do_word(Word_info);
    Lyric_walker(Lyric_staff* lstaff_l);
    Lyric_column* lcol_l();
    Lyric_staff* lstaff_l();
};


#endif // LYRICWALKER_HH


