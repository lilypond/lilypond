/*
  timing-translator.hh -- declare Timing_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIMING_TRANSLATOR_HH
#define TIMING_TRANSLATOR_HH

#include "moment.hh"
#include "translator.hh"

#include "parray.hh"

class Timing_translator : public virtual Translator
{
public:
  VIRTUAL_COPY_CONS(Translator);
  Time_signature_change_req * time_signature_req_l () const;
  Timing_translator ();
  Link_array<Timing_req> timing_req_l_arr_;

protected: 
  virtual void do_creation_processing ();
  virtual bool do_try_music (Music *req_l);
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
  virtual void do_post_move_processing();

public:

  Moment measure_position () const;
  Moment measure_length () const;  
  void set_time_signature (int, int);
  void get_time_signature (int *, int*) const;

};
#endif // TIMING_TRANSLATOR_HH
