/*
  bar-grav.hh -- declare Bar_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BARGRAV_HH
#define BARGRAV_HH
#include "engraver.hh"

/**
  generate bars. Either user ("|:"), or default (new measure)
  */
class Bar_engraver : public Engraver {
  Bar_req * bar_req_l_;
  Bar * bar_p_;

  void create_bar ();
public:
  TRANSLATOR_CLONE(Bar_engraver);
  Bar_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;

     
protected:
  virtual void do_creation_processing ();
  virtual void do_removal_processing ();
  virtual bool do_try_request (Request *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
};

#endif // BARGRAV_HH
