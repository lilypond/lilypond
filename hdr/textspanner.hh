/*
  textspanner.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef TEXTSPANNER_HH
#define TEXTSPANNER_HH

#include "string.hh"
#include "directionalspanner.hh"

/// a spanner which puts texts on top of other spanners.
struct Text_spanner : Spanner {
    int align;
    String text;
    String style;
    Directional_spanner*support;
    /****************/
    virtual    void process();
    virtual    void preprocess();
    virtual    Interval height() const;
    virtual Spanner* broken_at(PCol*,PCol*)const;
    Text_spanner(Directional_spanner*);
};
/**
  Use for triplets, eentweetjes, ottava, etc.
  */

#endif // TEXTSPANNER_HH

