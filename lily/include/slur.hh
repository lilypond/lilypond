/*
  slur.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "directional-spanner.hh"
#include "lily-proto.hh"
#include "varray.hh"
#include "bow.hh"

class Slur : public Bow {
public:
    Array<Note_column*> encompass_arr_;

    void do_post_processing();
    void do_pre_processing();
    void add(Note_column*);
    void set_default_dir();

    Spanner* do_break_at( PCol*, PCol*) const; 
private:
    NAME_MEMBERS(Slur);
};

#endif // SLUR_HH


