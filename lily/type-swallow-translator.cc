/*
  type-swallow-translator.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator.hh"
#include "event.hh"

class Skip_event_swallow_translator : public virtual Translator
{
protected:
  virtual bool try_music (Music*) { return true; }

public:  
  TRANSLATOR_DECLARATIONS(Skip_event_swallow_translator);
};


class Rest_swallow_translator : public virtual Translator
{
protected:
  virtual bool try_music (Music*) { return true; }

public:  
  TRANSLATOR_DECLARATIONS(Rest_swallow_translator);
};


Skip_event_swallow_translator::Skip_event_swallow_translator(){}

ENTER_DESCRIPTION(Skip_event_swallow_translator,
		  "Swallow \\skip.",
		  "",
		  "skip-event",
		  "",
		  "",
		  "");


Rest_swallow_translator::Rest_swallow_translator(){}

ENTER_DESCRIPTION(Rest_swallow_translator,
		  "Swallow rest",
		  "",
		  "rest-event",
		  "",
		  "",
	

	  "");

class Note_swallow_translator : public virtual Translator
{
protected:
  virtual bool try_music (Music*) { return true; }

public:  
  TRANSLATOR_DECLARATIONS(Note_swallow_translator);
};

Note_swallow_translator::Note_swallow_translator(){}

ENTER_DESCRIPTION(Note_swallow_translator,
		  "Swallow notes",
		  "",
		  "note-event",
		  "",
		  "",
		  "");
		  
