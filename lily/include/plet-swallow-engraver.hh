/*
  plet-swallow-engraver.hh -- declare Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef PLET_SWALLOW_ENGRAVER_HH
#define PLET_SWALLOW_ENGRAVER_HH

#include "swallow-grav.hh"

/**
  This engraver swallows plets silently.
 */
class Plet_swallow_engraver : public Swallow_engraver 
{
public:
  TRANSLATOR_CLONE(Plet_swallow_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;

protected:
  virtual bool do_try_request (Request*);
};

#endif // PLET_SWALLOW_ENGRAVER_HH
