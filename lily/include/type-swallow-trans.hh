/*
  type-swallow-engraver.hh -- declare Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TYPESWALLOW_GRAV_HH
#define TYPESWALLOW_GRAV_HH

#include "translator.hh"

/// eat a certain type of request
class Type_swallow_translator : public virtual Translator
{
protected:
  const type_info * type_;
  bool do_try_music (Music*);
public:
  
  VIRTUAL_COPY_CONS(Translator);
  Type_swallow_translator ();
};

#define DECLARE_REQUEST_SWALLOWER(TYPE)  \
struct TYPE ## _swallow_translator : public Type_swallow_translator {\
  TYPE ## _swallow_translator() { \
    type_ = &typeid (TYPE);\
  }\
  \
  VIRTUAL_COPY_CONS(Translator);\
};\
ADD_THIS_TRANSLATOR(TYPE ## _swallow_translator);\

#endif // TYPESWALLOW_GRAV_HH

