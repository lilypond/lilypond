/*
  meter-grav.hh -- declare  Meter_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef METERGRAV_HH
#define METERGRAV_HH
#include "engraver.hh"
#include "time-description.hh"
#include "grouping.hh"

/**
  generate meters. 
  */
class Meter_engraver : public Engraver {
protected:
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Meter_engraver);
  Meter * meter_p_;

  Meter_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // METERGRAV_HH
