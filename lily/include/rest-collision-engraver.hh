/*
  rest-collision-engraver.hh -- declare Rest_collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_COLLISION_GRAV_HH
#define REST_COLLISION_GRAV_HH

#include "array.hh"
#include "engraver.hh"

class Rest_collision_engraver : public Engraver {
  Rest_collision* rest_collision_p_;

  Link_array<Note_column> note_column_l_arr_;
protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Rest_collision_engraver();
  
};
#endif // REST_COLLISION_GRAV_HH
