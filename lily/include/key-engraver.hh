/*
  key-engraver.hh -- declare Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef KEYGRAV_HH
#define KEYGRAV_HH

#include "engraver.hh"
#include "key.hh"
#include "musical-pitch.hh"

/**
  Make the key signature.
 */
class Key_engraver : public Engraver {
  void create_key();
  void read_req (Key_change_req const * r);

public:
  Key_engraver();
  
  VIRTUAL_COPY_CONS(Translator);
  Key key_;
  Key_change_req * keyreq_l_;
  Key_item * kit_p_;
  Array<Musical_pitch> accidental_idx_arr_;
  Array<Musical_pitch> old_accidental_idx_arr_;
  bool default_key_b_;
  bool change_key_b_;
    
protected:
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void acknowledge_element (Score_element_info);
};

#endif // KEYGRAV_HH
