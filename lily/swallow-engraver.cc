/*
  swallow-engraver.cc -- implement Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

class Swallow_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Swallow_engraver);
protected:
  bool try_music (Music *);
};

bool
Swallow_engraver::try_music (Music *)
{
  return true;
}

Swallow_engraver::Swallow_engraver ()
{
}

#include "translator.icc"

ADD_TRANSLATOR (Swallow_engraver,
		/* doc */
		"This engraver swallows everything given to it silently."
		"  The purpose of this is to prevent spurious @q{event junked}"
		" warnings.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);
