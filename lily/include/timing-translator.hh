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

  SCM last_time_sig_;
public:
  VIRTUAL_COPY_CONS(Translator);
  Timing_translator ();
  Music *check_;

protected: 
  virtual void do_creation_processing ();
  virtual bool try_music (Music *req_l);
  void deprecated_process_music();
  virtual void stop_translation_timestep();
  virtual void start_translation_timestep();

public:
  Moment measure_position () const;
  Moment measure_length () const;  
  void set_time_signature ();
};
#endif // TIMING_TRANSLATOR_HH
