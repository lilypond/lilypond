/*
  collision-grav.hh -- declare Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef COLLISION_GRAV_HH
#define COLLISION_GRAV_HH

#include "engraver.hh"

class Collision_engraver : public Engraver {
  Collision* col_p_;

protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Collision_engraver);
  Collision_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // COLLISION_GRAV_HH
