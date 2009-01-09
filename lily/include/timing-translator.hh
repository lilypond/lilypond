/*
  timing-translator.hh -- declare Timing_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TIMING_TRANSLATOR_HH
#define TIMING_TRANSLATOR_HH

#include "moment.hh"
#include "translator.hh"

class Timing_translator : public Translator
{
public:
  TRANSLATOR_DECLARATIONS (Timing_translator);

protected:
  virtual void initialize ();
  void stop_translation_timestep ();
  void start_translation_timestep ();

public:
  Rational measure_length () const;
};

#endif // TIMING_TRANSLATOR_HH
