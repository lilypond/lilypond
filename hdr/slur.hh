/*
  slur.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "directionalspanner.hh"
#include "fproto.hh"
#include "varray.hh"

struct Slur : Directional_spanner {

    Array<Notehead*> encompass;
    //int dir;

    bool open_left,open_right;			

    /****************/
    Offset center() const;
    Slur();
    void print() const;    
    void do_post_processing();
    void do_pre_processing();
    void add(Notehead*);
    void set_default_dir();

    Spanner* do_break_at( PCol*, PCol*) const;
    void process();
private:
Molecule*brew_molecule()const;
};

#endif // SLUR_HH


