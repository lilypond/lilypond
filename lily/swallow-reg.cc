/*
  swallow-reg.cc -- implement Swallow_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "swallow-reg.hh"

IMPLEMENT_STATIC_NAME(Swallow_register);
IMPLEMENT_IS_TYPE_B1(Swallow_register,Request_register);
ADD_THIS_REGISTER(Swallow_register);


bool
Swallow_register::do_try_request(Request*) 
{
    return true;
}
