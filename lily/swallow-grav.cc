/*
  swallow-reg.cc -- implement Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "swallow-grav.hh"


IMPLEMENT_IS_TYPE_B1(Swallow_engraver,Engraver);
ADD_THIS_TRANSLATOR(Swallow_engraver);


bool
Swallow_engraver::do_try_request (Request*) 
{
  return true;
}
