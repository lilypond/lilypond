/*
  score-align-gravs.cc -- implement different alignment engravers.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "key-item.hh"
#include "clef-item.hh"
#include "meter.hh"
#include "bar.hh"
#include "score-align-grav.hh"

#define IMPLEMENT_ALIGN_GRAV(C,T,p)\
class C ## _align_engraver : public Type_align_engraver		\
{									\
public:									\
  DECLARE_MY_RUNTIME_TYPEINFO;					\
  TRANSLATOR_CLONE(C ## _align_engraver);\
  C ## _align_engraver() : Type_align_engraver () \
  { type_ch_C_ = T::static_name();\
  priority_i_ = p;}	\
};									\
ADD_THIS_TRANSLATOR(C ## _align_engraver);				\
IMPLEMENT_IS_TYPE_B1(C ## _align_engraver, Type_align_engraver)    	;


IMPLEMENT_ALIGN_GRAV(Key,Key_item,3);
IMPLEMENT_ALIGN_GRAV(Clef,Clef_item,2);
IMPLEMENT_ALIGN_GRAV(Meter,Meter,4);
IMPLEMENT_ALIGN_GRAV(Bar, Bar,0);

