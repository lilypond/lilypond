//
//  lyricstaff.hh -- part of LilyPond
//
//  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>

#ifndef LYRICSTAFF_HH
#define LYRICSTAFF_HH

#include "staff.hh"

/// (lstaff)
struct Lyric_staff : Staff {
    PStaff* pstaff_l_;

    Staff_column* create_col(Score_column*);

    virtual void set_output(PScore *);
    void process_commands(PCursor<Command*> &where);
    virtual void walk();

    Lyric_staff();
};

#endif // LYRICSTAFF_HH




