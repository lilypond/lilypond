/*
  swallow-performer.cc -- implement Swallow_performer

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "performer.hh"
#include "music.hh"

class Swallow_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Swallow_performer);
protected:
  virtual bool try_music (Music *);
};

bool
Swallow_performer::try_music (Music *m)
{
  if (m->is_mus_type ("melisma-playing-event"))
    return false;
  else
    return true;
}

Swallow_performer::Swallow_performer ()
{}

#include "translator.icc"

ADD_TRANSLATOR (Swallow_performer,
		/* doc */
		"",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
