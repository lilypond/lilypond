/*
  swallow-perf.cc -- implement Swallow_performer

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "performer.hh"

class Swallow_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS(Swallow_performer);
protected:
  virtual bool try_music (Music*) { return true; }
};

Swallow_performer::Swallow_performer()
{}

ENTER_DESCRIPTION(Swallow_performer,
/* descr */       "",
/* creats*/       "",
/* accepts */     "general-music",
/* acks  */      "",
/* reads */       "",
/* write */       "");
