/*
  time_signature-engraver.hh -- declare  Time_signature_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef METERGRAV_HH
#define METERGRAV_HH
#include "engraver.hh"
#include "time-description.hh"
#include "grouping.hh"

/**
  generate time_signatures. 
  */
class Time_signature_engraver : public Engraver {
protected:
  virtual void do_process_requests();
  virtual void do_pre_move_processing();
public:
  TRANSLATOR_CLONE(Time_signature_engraver);
  Time_signature * time_signature_p_;

  Time_signature_engraver();
  DECLARE_MY_RUNTIME_TYPEINFO;
};
#endif // METERGRAV_HH
