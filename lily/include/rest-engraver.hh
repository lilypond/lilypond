/*
  rest-engraver.hh -- declare Engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_GRAV_HH
#define REST_GRAV_HH

#include "engraver.hh"

class Rest_engraver : public Engraver
{
  Rest_req *rest_req_l_;
  Dots * dot_p_;
  Rest * rest_p_;
protected:
  virtual bool do_try_music (Music *);
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void do_process_requests ();
public:
  
  VIRTUAL_COPY_CONS(Translator);
  Rest_engraver ();
};
#endif // REST_GRAV_HH
