/*
  timing-grav.hh -- declare Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
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
  virtual void acknowledge_element (Score_elem_info i);
  virtual void fill_staff_info (Staff_info&);
  virtual Engraver * engraver_l () { return Engraver::engraver_l (); }
public:
  TRANSLATOR_CLONE(Timing_engraver);
  DECLARE_MY_RUNTIME_TYPEINFO;
};

#endif // TIMING_GRAV_HH
