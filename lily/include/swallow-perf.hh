/*
  swallow-perf.hh -- declare Swallow_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SWALLOW_PERF_HH
#define SWALLOW_PERF_HH

#include "performer.hh"

class Swallow_performer : public Performer
{
public:
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual bool try_music (Music *) { return true; }
};

#endif // SWALLOW_PERF_HH
