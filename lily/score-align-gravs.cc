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

#define IMPLEMENT_ALIGN_REG(C,T,p)\
class C ## _align_engraver : public Score_align_engraver		\
{									\
public:									\
    DECLARE_MY_RUNTIME_TYPEINFO;					\
    C ## _align_engraver() : Score_align_engraver() \
    { type_ch_C_ = T::static_name();\
    priority_i_ = p;}	\
};									\
ADD_THIS_ENGRAVER(C ## _align_engraver);				\
IMPLEMENT_IS_TYPE_B1(C ## _align_engraver, Score_align_engraver)    	;


IMPLEMENT_ALIGN_REG(Key,Key_item,3);
IMPLEMENT_ALIGN_REG(Clef,Clef_item,2);
IMPLEMENT_ALIGN_REG(Meter,Meter,4);
IMPLEMENT_ALIGN_REG(Bar, Bar,0);

