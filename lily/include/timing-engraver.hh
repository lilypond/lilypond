/*
  timing-engraver.hh -- declare Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIMING_GRAV_HH
#define TIMING_GRAV_HH

#include "timing-translator.hh"

/**
  Do time bookkeeping
 */
class Timing_engraver : public Timing_translator, public Engraver
{   
protected:
  virtual void fill_staff_info (Staff_info&);
  virtual Engraver * access_Engraver () { return Engraver::access_Engraver (); }
public:
  TRANSLATOR_CLONE(Timing_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // TIMING_GRAV_HH
