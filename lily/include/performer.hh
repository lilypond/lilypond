/*
  performer.hh -- declare Performer

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
                 Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "lily-proto.hh"
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
  TRANSLATOR_CLONE(Performer);
  DECLARE_MY_RUNTIME_TYPEINFO;
  Performer_group_performer* daddy_perf_l() const;
protected:
  virtual int get_tempo_i() const;
  virtual void play (Audio_element * elem_p );
  Performer * access_Performer () { return this; }
};


#endif // PERFORMER_HH
