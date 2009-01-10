/*
  type-swallow-translator.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "translator.hh"
#include "music.hh"

#include "translator.icc"

class Skip_event_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Skip_event_swallow_translator);
};

class Rest_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Rest_swallow_translator);
};

Skip_event_swallow_translator::Skip_event_swallow_translator ()
{
}

ADD_TRANSLATOR (Skip_event_swallow_translator,
		/* doc */
		"Swallow @code{\\skip}.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

Rest_swallow_translator::Rest_swallow_translator (){}

ADD_TRANSLATOR (Rest_swallow_translator,
		/* doc */
		"Swallow rest.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

class Note_swallow_translator : public Translator
{
protected:
  virtual bool try_music (Music *) { return true; }

public:
  TRANSLATOR_DECLARATIONS (Note_swallow_translator);
};

Note_swallow_translator::Note_swallow_translator ()
{
}

ADD_TRANSLATOR (Note_swallow_translator,
		/* doc */
		"Swallow notes.",

		/* create */
		"",

		/* read */
		"",

		/* write */
		""
		);

