/*
  performer.hh -- declare Performer

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "audio-element-info.hh"
#include "array.hh"
#include "request.hh"
#include "score-element-info.hh"
#include "staff-info.hh"
#include "translator.hh"

/**
  Convert a music definition into a audio representation.
  A baseclass
 */
class Performer : public virtual Translator{
public:
  VIRTUAL_COPY_CONS(Translator);
  friend class Performer_group_performer;  
  Performer_group_performer* daddy_perf_l() const;
protected:
  virtual void announce_element (Audio_element_info);
  virtual void acknowledge_element (Audio_element_info);
  virtual void process_acknowledged ();
  virtual int get_tempo_i() const;
  virtual void play (Audio_element * elem_p );
};


#endif // PERFORMER_HH

