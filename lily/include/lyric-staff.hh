/*
  lyricstaff.hh -- part of LilyPond

  copyright 1997 Jan Nieuwenhuizen <jan@digicash.com>
  */

#ifndef LYRICSTAFF_HH
#define LYRICSTAFF_HH

#include "staff.hh"

/**
  Hungarian prefix lstaff
 */
struct Lyric_staff : Staff {
    virtual void set_output(PScore *);
    virtual Staff_walker *get_walker_p();
};

#endif // LYRICSTAFF_HH




