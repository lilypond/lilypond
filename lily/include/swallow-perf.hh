/*
  swallow-perf.hh -- declare Swallow_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SWALLOW_PERF_HH
#define SWALLOW_PERF_HH

#include "performer.hh"

class Swallow_performer : public Performer {
public:
    NAME_MEMBERS();
    virtual bool try_request (Request* ) { return true; }
};

#endif // SWALLOW_PERF_HH
