/*
  swallow-perf.cc -- implement Swallow_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "performer.hh"

class Swallow_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Swallow_performer);
protected:
  virtual bool try_music (Music*);
};

bool
Swallow_performer::try_music (Music *m)
{
  if (m->is_mus_type ("busy-playing-event")
      || m->is_mus_type ("melisma-playing-event"))
    return false;
  else
    return true; 
}

Swallow_performer::Swallow_performer ()
{}

ADD_TRANSLATOR (Swallow_performer,
/* descr */       "",
/* creats*/       "",
/* accepts */     "general-music",
/* acks  */      "",
/* reads */       "",
/* write */       "");
