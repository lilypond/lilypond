/*
  timing-translator.hh -- declare Timing_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIMING_TRANSLATOR_HH
#define TIMING_TRANSLATOR_HH

#include "moment.hh"
#include "translator.hh"

#include "parray.hh"

class Timing_translator : public virtual Translator
{
public:
  VIRTUAL_COPY_CONS (Translator);
  Timing_translator ();

protected: 
  virtual void initialize ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();

public:
  Moment measure_position () const;
  Rational measure_length () const;  
};
#endif // TIMING_TRANSLATOR_HH
