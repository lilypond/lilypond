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
  const char * type_;
  bool do_try_request (Request*);
public:
  DECLARE_MY_RUNTIME_TYPEINFO;
  TRANSLATOR_CLONE(Type_swallow_translator);
  Type_swallow_translator ();
};

#define DECLARE_REQUEST_SWALLOWER(TYPE)  \
struct TYPE ## _swallow_translator : public Type_swallow_translator {\
  TYPE ## _swallow_translator() { \
    type_ = TYPE::static_name ();\
  }\
  DECLARE_MY_RUNTIME_TYPEINFO;\
  TRANSLATOR_CLONE(TYPE ## _swallow_translator);\
};\
IMPLEMENT_IS_TYPE_B1(TYPE ## _swallow_translator, Type_swallow_translator);\
ADD_THIS_TRANSLATOR(TYPE ## _swallow_translator);\

#endif // TYPESWALLOW_GRAV_HH

