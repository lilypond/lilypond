/*
  slur.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef SLUR_HH
#define SLUR_HH

#include "directionalspanner.hh"
#include "fproto.hh"
#include "vray.hh"

struct Slur : Directional_spanner {

    svec<Notehead*> encompass;
    //int dir;

    bool open_left,open_right;			

    /****************/
    Offset center() const;
    void calculate();
    void print()const;    
    void preprocess();
    void add(Notehead*);
    void set_default_dir();
    Interval height() const;
    Spanner* broken_at( PCol*, PCol*) const;
    void process();
private:
    void brew_molecule();
};

#endif // SLUR_HH


