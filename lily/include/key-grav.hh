/*
  key-grav.hh -- declare Key_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef KEYGRAV_HH
#define KEYGRAV_HH

#include "engraver.hh"
#include "key.hh"

/**
  Make the key signature.
 */
class Key_engraver : public Engraver {
  void create_key();
  void read_req (Key_change_req * r);

public:
  Key_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
  TRANSLATOR_CLONE(Key_engraver);
  Key key_;
  Key_change_req * keyreq_l_;
  Key_item * kit_p_;
  Array<int> accidental_idx_arr_;
  bool default_key_b_;
  bool change_key_b_;
    
protected:
  virtual bool do_try_request (Request *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void acknowledge_element (Score_elem_info);
};

#endif // KEYGRAV_HH
