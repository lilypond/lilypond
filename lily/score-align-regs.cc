/*
  score-align-regs.cc -- implement different alignment registers.

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "key-item.hh"
#include "clef-item.hh"
#include "meter.hh"
#include "bar.hh"
#include "score-align-reg.hh"

#define IMPLEMENT_ALIGN_REG(C,T,p)\
class C ## _align_register : public Score_align_register		\
{									\
public:									\
    NAME_MEMBERS();							\
    C ## _align_register() : Score_align_register() \
    { type_ch_C_ = T::static_name();\
    priority_i_ = p;}	\
};									\
IMPLEMENT_STATIC_NAME(C ## _align_register)	;			\
ADD_THIS_REGISTER(C ## _align_register);				\
IMPLEMENT_IS_TYPE_B1(C ## _align_register, Score_align_register)    	;


IMPLEMENT_ALIGN_REG(Key,Key_item,3);
IMPLEMENT_ALIGN_REG(Clef,Clef_item,2);
IMPLEMENT_ALIGN_REG(Meter,Meter,4);
IMPLEMENT_ALIGN_REG(Bar, Bar,0);

