/*
  swallow-reg.cc -- implement Swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "swallow-grav.hh"

IMPLEMENT_STATIC_NAME(Swallow_engraver);
IMPLEMENT_IS_TYPE_B1(Swallow_engraver,Request_engraver);
ADD_THIS_ENGRAVER(Swallow_engraver);


bool
Swallow_engraver::do_try_request(Request*) 
{
    return true;
}
