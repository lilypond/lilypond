/*
  directionalspanner.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef DIRECTIONALSPANNER_HH
#define DIRECTIONALSPANNER_HH

#include "spanner.hh"

struct Directional_spanner : Spanner{
    
    /// -1 below heads, +1 above heads.
    int dir;

    /// offset of "center" relative to left-column/0-pos of staff
    virtual Offset center() const=0;
    virtual void set_default_dir()=0;
    Directional_spanner();
    
};

#endif // DIRECTIONALSPANNER_HH

