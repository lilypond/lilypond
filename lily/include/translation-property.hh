/*
  translation-property.hh -- declare Translation_property

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TRANSLATION_PROPERTY_HH
#define TRANSLATION_PROPERTY_HH

#include "music.hh"
#include "scalar.hh"

/**
  Set a property of Translator 
 */
class Translation_property : public Music
{
public:
  String var_str_;
  Scalar value_;
  VIRTUAL_COPY_CONS(Music);
  
protected:
  virtual void do_print () const;
};

#endif // PROPERTY_HH
