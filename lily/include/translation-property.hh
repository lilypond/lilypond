/*
  translation-property.hh -- declare Translation_property

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATION_PROPERTY_HH
#define TRANSLATION_PROPERTY_HH

#include "music.hh"


/**
  Set a property of Translator

  value -- the value to set
  symbol -- the symbol to set.

*/
class Translation_property : public Music
{
public:
  VIRTUAL_COPY_CONS(Music);
};

#endif // PROPERTY_HH
