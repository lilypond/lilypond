/*
  collision.hh -- declare Collision

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_HH
#define COLLISION_HH
#include "lily-proto.hh"
#include "item.hh"


/**
  Resolve conflicts between various Note_columns (chords).
  
  TODO 

  multistaff support (see Chlapik: equal noteheads should be on the
  same hpos.)  
*/
class Collision : public Item
{
protected:
  SCM automatic_shift ();
  SCM forced_shift ();
  
  virtual void before_line_breaking ();
public:
    
  void add_column (Note_column*ncol_l);
  Collision();
};
#endif // COLLISION_HH
