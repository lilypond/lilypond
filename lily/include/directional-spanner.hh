/*
  directional-spanner.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef DIRECTIONALSPANNER_HH
#define DIRECTIONALSPANNER_HH

#include "spanner.hh"

/** a spanner which can be pointing "up" or "down".

    JUNKME?
 */
class Directional_spanner : public Spanner{
  /// -1 below heads, +1 above heads.
  Direction dir_;

public:
  Directional_spanner();

  void set_direction (Direction d ) { dir_ =  d; }
  Direction get_direction () const { return dir_; }
  
  /// offset of "center" relative to left-column/0-pos of staff
  virtual Offset center() const;
  virtual Direction get_default_dir() const;
protected:
  virtual void do_pre_processing();
};

#endif // DIRECTIONALSPANNER_HH

