/*
  stem.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef STEM_HH
#define STEM_HH
#include "item.hh"

struct Stem : public Item {
    // heads the stem encompasses (positions)
    int minnote, maxnote;

    int staff_center;

    // extent of the stem (positions)
    int bot, top;
    
    // flagtype? 4 none, 8 8th flag, 0 = beam.
    int flag;
    
        
    /****************/
    void brew_molecole();
    void calculate();
    Stem(int center);
    void print() const;
    Interval width() const;    
};
#endif
