/*
  textspanner.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef TEXTSPANNER_HH
#define TEXTSPANNER_HH

#include "string.hh"
#include "directionalspanner.hh"
#include "textdef.hh"

/// a spanner which puts texts on top of other spanners.
struct Text_spanner : Spanner {
    Text_def spec;
    Offset tpos;
    Directional_spanner*support;

    /****************/

    const char * name() const;
    virtual    void do_pre_processing();
    virtual    void do_post_processing();
    Molecule* brew_molecule_p()const;
    virtual    Interval height() const ;
    void do_print() const;
    virtual Spanner* do_break_at(PCol*,PCol*)const;
    Text_spanner();
    void set_support(Directional_spanner*);
};
/**
  Use for triplets, eentweetjes, ottava, etc.
  */

#endif // TEXTSPANNER_HH

