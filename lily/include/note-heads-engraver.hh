/*
  head-engraver.hh -- part of GNU LilyPond

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  Link_array<Dots> dot_p_arr_;
  Link_array<Note_req> note_req_l_arr_;
  PQueue<Moment> notes_end_pq_;

public:
  VIRTUAL_COPY_CONS(Translator);
  Note_heads_engraver();
  
protected:
  virtual bool do_try_music (Music *req_l) ;
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
};


#endif // HEADSGRAV_HH
