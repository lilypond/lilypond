/*
  slur.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "directional-spanner.hh"
#include "fproto.hh"
#include "varray.hh"

struct Slur : Directional_spanner {

    Array<Notehead*> encompass;

    bool open_left, open_right;			

    /* *************** */
    Offset center() const;
    Slur();
    void do_post_processing();
    void do_pre_processing();
    void add(Notehead*);
    void set_default_dir();

    Spanner* do_break_at( PCol*, PCol*) const;
    void process();
private:
    Molecule*brew_molecule_p()const;
    NAME_MEMBERS(Slur);
};

#endif // SLUR_HH


