/*
  swallow-grav.hh -- declare Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef SWALLOW_GRAV_HH
#define SWALLOW_GRAV_HH

#include "engraver.hh"

/**
  This engraver swallows everything given to it silently. The purpose of
  this is to prevent spurious "request junked" warnings.
 */
class Swallow_engraver : public Engraver {
protected:
  bool acceptable_request_b (Request*) const;
  bool do_try_request (Request*) ;
public:
  TRANSLATOR_CLONE(Swallow_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // SWALLOW_GRAV_HH
