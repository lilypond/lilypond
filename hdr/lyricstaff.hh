/*
  lyricstaff.hh -- part of LilyPond

  copyright 1997 Jan Nieuwenhuizen <jan@digicash.nl>
  */

#ifndef LYRICSTAFF_HH
#define LYRICSTAFF_HH

#include "staff.hh"

/**
  Hungarian prefix lstaff
 */
struct Lyric_staff : Staff {
    PStaff* pstaff_l_;

    Staff_column* create_col();
    virtual void set_output(PScore *);
    virtual Staff_walker *get_walker_p();
    Lyric_staff();
};

#endif // LYRICSTAFF_HH




