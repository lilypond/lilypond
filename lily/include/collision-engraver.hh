/*
  collision-engraver.hh -- declare Collision_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef COLLISION_GRAV_HH
#define COLLISION_GRAV_HH

#include "engraver.hh"

class Collision_engraver : public Engraver {
  Collision* col_p_;
  Link_array<Note_column> note_column_l_arr_;

protected:
  virtual void acknowledge_element (Score_element_info);
  virtual void process_acknowledged ();
  virtual void do_pre_move_processing();
public:
  VIRTUAL_COPY_CONS(Translator);
  Collision_engraver();
  
};
#endif // COLLISION_GRAV_HH
