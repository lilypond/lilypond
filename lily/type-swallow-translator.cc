/*
  type-swallow-translator.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "translator.hh"
#include "musical-request.hh"

class Skip_req_swallow_translator : public virtual Translator
{
protected:
  virtual bool try_music (Music*) { return true; }

public:  
  TRANSLATOR_DECLARATIONS(Skip_req_swallow_translator);
};


Skip_req_swallow_translator::Skip_req_swallow_translator(){}

ENTER_DESCRIPTION(Skip_req_swallow_translator,
		  "Swallow \\skip.",
		  "",
		  "skip-event",
		  "",
		  "",
		  "");
		  
