/*
  slur-engraver.hh -- declare Slur_engraver

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SLURGRAV_HH
#define SLURGRAV_HH

#include "engraver.hh"

class Slur_engraver :public Engraver {
  Array<Slur_req*> requests_arr_;
  Array<Slur_req*> new_slur_req_l_arr_;
  Array<Slur *> slur_l_stack_;
  Array<Slur*> end_slur_l_arr_;

protected:
  virtual bool do_try_request (Request*);
  virtual void do_process_requests();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void do_removal_processing ();

public:
  TRANSLATOR_CLONE(Slur_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // SLURGRAV_HH
