/*
  performer.hh -- declare Performer

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
                 Jan Nieuwenhuizen <jan@digicash.com>
 */

#ifndef PERFORMER_HH
#define PERFORMER_HH

#include "lily-proto.hh"
#include "varray.hh"
#include "request.hh"
#include "score-elem-info.hh"
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
  Performer * performer_l () { return this; }
};


#endif // PERFORMER_HH
