/*
  slur-engraver.hh -- declare Slur_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SLURGRAV_HH
#define SLURGRAV_HH

#include "engraver.hh"

class Slur_engraver :public Engraver {
  Link_array<Span_req> requests_arr_;
  Link_array<Span_req> new_slur_req_l_arr_;
  Link_array<Slur> slur_l_stack_;
  Link_array<Slur> end_slur_l_arr_;

  void set_melisma (bool);
protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_requests();
  virtual void acknowledge_element (Score_element_info);
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();
  virtual void do_removal_processing ();

public:
  VIRTUAL_COPY_CONS(Translator);
  
};

#endif // SLURGRAV_HH
