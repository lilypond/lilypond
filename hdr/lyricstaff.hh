//
//  lyricstaff.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>

#ifndef LYRICSTAFF_HH
#define LYRICSTAFF_HH

#include "staff.hh"

/// (lstaff)
struct Lyric_staff : Staff {
    PStaff* line_pstaff_p_;

    Staff_column* create_col(Score_column*);
    
//    virtual Item *get_TYPESET_item(Command*);
    virtual void set_output(PScore *);
//    virtual Local_key_item* get_local_key_item();

    void process_commands(PCursor<Command*> &where);
    virtual void walk();

    Lyric_staff();
};

#endif // LYRICSTAFF_HH




