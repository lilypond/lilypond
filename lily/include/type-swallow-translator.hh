/*
  type-swallow-translator.hh -- declare Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef TYPESWALLOW_TRANSLATOR_HH
#define TYPESWALLOW_TRANSLATOR_HH

#include "translator.hh"

/** eat a certain type of event
    (Duh, it's good for your skin)
*/
class Type_swallow_translator : public Translator
{
protected:
  string swallow_string_;
  bool try_music (Music *);
public:
  VIRTUAL_COPY_CONS (Translator);
};

#define DECLARE_EVENT_SWALLOWER(TYPE)					\
  struct TYPE ## _swallow_translator : public Type_swallow_translator	\
  {									\
    TRANSLATOR_DECLARATIONS (TYPE ## _swallow_translator);		\
  };									\
  TYPE ## _swallow_translator ::TYPE ## _swallow_translator ()		\
  {									\
    swallow_string_ = #TYPE;						\
  }									\
  ADD_TRANSLATOR (TYPE ## _swallow_translator,				\
		  "Swallow events of " #TYPE " type.",			\
		  "",							\
		  "general-music",					\
		  "",							\
		  "",							\
		  "");

#endif // TYPESWALLOW_TRANSLATOR_HH

