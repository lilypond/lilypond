/*
  timing-translator.hh -- declare Timing_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIMING_TRANSLATOR_HH
#define TIMING_TRANSLATOR_HH

#include "translator.hh"
#include "time-description.hh"
#include "grouping.hh"
#include "parray.hh"

class Timing_translator : public virtual Translator
{
public:
  TRANSLATOR_CLONE(Timing_translator);
  Time_signature_change_req * time_signature_req_l () const;
  Timing_translator ();
  DECLARE_MY_RUNTIME_TYPEINFO;
  Time_description time_;
  Rhythmic_grouping  default_grouping_;
  Link_array<Timing_req> timing_req_l_arr_;
protected: 
  virtual void do_print () const;
  virtual void do_creation_processing ();
  virtual bool do_try_request (Request *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

};
#endif // TIMING_TRANSLATOR_HH
