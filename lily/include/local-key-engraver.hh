/*
  local-key-engraver.hh -- declare Local_key_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef LOCALKEYGRAV_HH
#define LOCALKEYGRAV_HH

#include "engraver.hh"
#include "key.hh"
#include "parray.hh"
/**
   Make accidentals.  Catches note heads, ties and notices key-change
   events.  Due to interaction with ties (which don't come together
   with note heads), this needs to be in a context higher than Tie_engraver.
   (FIXME).
 */
struct Local_key_engraver : Engraver {
  Local_key_item *key_item_p_;
protected:
  VIRTUAL_COPY_CONS(Translator);
  virtual void do_process_requests();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_creation_processing ();
  virtual void process_acknowledged ();
  virtual void do_removal_processing ();
public:
  
  Key local_key_;
  Key_engraver *key_grav_l_;
  Array<Note_req* > mel_l_arr_;
  Array<Item* > support_l_arr_;
  Link_array<Item  > forced_l_arr_;
  Link_array<Item > tied_l_arr_;
  Local_key_engraver();
  bool self_grace_b_;
  Grace_align_item * grace_align_l_;
};

#endif // LOCALKEYGRAV_HH
