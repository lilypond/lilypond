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
    DECLARE_MY_RUNTIME_TYPEINFO;
protected:
    virtual bool do_try_request (Request*) { return true; }
};

#endif // SWALLOW_PERF_HH
