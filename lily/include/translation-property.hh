#if 0
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
  Translation_property ();
  VIRTUAL_COPY_CONS(Music);
};

/**
   Push onto basic property list.
   
  symbols -- list of basic-property lists

  element-property -- element property name

  element-value -- element property value
  
 */
class Push_translation_property : public Music
{
public:
  VIRTUAL_COPY_CONS(Music);
};

/**
  Restore previous setting.

  symbols -- list of basic-property lists

  element-property -- element property name
 */
class Pop_translation_property : public Music
{
public:
  VIRTUAL_COPY_CONS(Music);
};


void apply_push_property (Translator_group*trans, SCM syms, SCM eprop, SCM val);
void apply_pop_property (Translator_group*trans, SCM syms, SCM eprop);


#endif // PROPERTY_HH
#endif
