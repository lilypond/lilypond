/*
  directional-spanner.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef DIRECTIONALSPANNER_HH
#define DIRECTIONALSPANNER_HH

#include "spanner.hh"

/// a spanner which can be pointing "up" or "down"
class Directional_spanner : public Spanner{
public:
    
    /// -1 below heads, +1 above heads.
    Direction dir_;
    Directional_spanner();
    
    /// offset of "center" relative to left-column/0-pos of staff
    virtual Offset center() const;
    virtual void set_default_dir();
protected:
    virtual void do_pre_processing();
};

#endif // DIRECTIONALSPANNER_HH

