/*
  head-grav.hh -- part of GNU LilyPond

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef HEADGRAV_HH
#define HEADGRAV_HH
#include "engraver.hh"

/**
  make balls and rests
 */
class Note_head_engraver : public Engraver {
  Note_head* note_p_;
  Dots * dot_p_;
  Rhythmic_req * note_req_l_;
    
public:
  TRANSLATOR_CLONE(Note_head_engraver);
  Note_head_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual bool do_try_request (Request *req_l) ;
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

};


#endif // HEADGRAV_HH
