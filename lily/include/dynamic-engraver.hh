/*
  dynamic-engraver.hh -- declare Dynamic_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef DYNAMIC_GRAV_HH
#define DYNAMIC_GRAV_HH

#include "engraver.hh"

class Dynamic_engraver : public Engraver {
  Direction dir_;
  Text_item * dynamic_p_;
  Crescendo * to_end_cresc_p_;
  Crescendo * cresc_p_;
  Span_dynamic_req * cresc_req_l_;
  Array<Dynamic_req*> dynamic_req_l_arr_;
public:
  TRANSLATOR_CLONE(Dynamic_engraver);
  Dynamic_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
protected:
  virtual void do_removal_processing ();
  virtual void acknowledge_element (Score_element_info);
  virtual bool do_try_request (Request *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
};

#endif // DYNAMIC_GRAV_HH
