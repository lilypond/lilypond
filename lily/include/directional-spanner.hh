/*
  directional-spanner.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef DIRECTIONALSPANNER_HH
#define DIRECTIONALSPANNER_HH

#include "spanner.hh"
#include "directional-element.hh"

/** a spanner which can be pointing "up" or "down".

    JUNKME
 */
class Directional_spanner : public Spanner, public Directional_element {
public:
  /// offset of "center" relative to left-column/0-pos of staff
  virtual Offset center() const;
  virtual Direction get_default_dir() const;
  VIRTUAL_COPY_CONS(Score_element);
protected:
  virtual void do_pre_processing();
};

#endif // DIRECTIONALSPANNER_HH

