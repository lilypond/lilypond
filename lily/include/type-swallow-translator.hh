/*
  type-swallow-engraver.hh -- declare Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TYPESWALLOW_GRAV_HH
#define TYPESWALLOW_GRAV_HH

#include "translator.hh"

/** eat a certain type of request.
    (Duh, it's good for your skin)
 */
class Type_swallow_translator : public virtual Translator
{
protected:
  String swallow_str_;
  bool try_music (Music*);
public:  
  VIRTUAL_COPY_CONS(Translator);
};

#define DECLARE_REQUEST_SWALLOWER(TYPE)  \
struct TYPE ## _swallow_translator : public Type_swallow_translator {\
  TYPE ## _swallow_translator() { \
      swallow_str_ =  #TYPE;\
  }\
  \
  VIRTUAL_COPY_CONS(Translator);\
};\
ADD_THIS_TRANSLATOR(TYPE ## _swallow_translator);\

#endif // TYPESWALLOW_GRAV_HH

