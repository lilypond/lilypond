/*
  rest-collision-grav.hh -- declare Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REST_COLLISION_GRAV_HH
#define REST_COLLISION_GRAV_HH

#include "varray.hh"
#include "engraver.hh"

class Rest_collision_engraver : public Engraver {
  Rest_collision* rest_collision_p_;

protected:
  virtual void acknowledge_element (Score_elem_info);
  virtual void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Rest_collision_engraver);
  Rest_collision_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // REST_COLLISION_GRAV_HH
