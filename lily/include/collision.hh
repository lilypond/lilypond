/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH
#include "lily-proto.hh"
#include "axis-group-item.hh"
#include "tuple.hh"

//junkme, use SCM conses.
typedef Tuple<Score_element*, Real> Shift_tup;

/**
  Resolve conflicts between various Note_columns (chords).
  
  TODO 

  multistaff support (see Chlapik: equal noteheads should be on the
  same hpos.)  
*/
class Collision : public Axis_group_item {
protected:
  Array<Shift_tup> automatic_shift ();
  Array<Shift_tup> forced_shift ();
  
  virtual void before_line_breaking ();
public:
    
  void add_column (Note_column*ncol_l);
  Collision();
};
#endif // COLLISION_HH
