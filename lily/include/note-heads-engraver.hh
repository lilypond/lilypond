/*
  head-engraver.hh -- part of GNU LilyPond

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef HEADSGRAV_HH
#define HEADSGRAV_HH
#include "engraver.hh"
#include "pqueue.hh"

/**
  make balls and rests
 */
class Note_heads_engraver : public Engraver {
  Link_array<Note_head> note_p_arr_;
  Link_array<Item> dot_p_arr_;
  Link_array<Note_req> note_req_l_arr_;
  Moment note_end_mom_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Note_heads_engraver();
  
protected:
  virtual bool do_try_music (Music *req_l) ;
  virtual void do_process_music();
  virtual void do_pre_move_processing();
};


#endif // HEADSGRAV_HH
