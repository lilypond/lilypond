/*
  swallow-reg.cc -- implement Swallow_register

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "swallow-reg.hh"

IMPLEMENT_STATIC_NAME(Swallow_register);
ADD_THIS_REGISTER(Swallow_register);

bool
Swallow_register::acceptable_request_b(Request*) const
{
    return true;
}

bool
Swallow_register::try_request(Request*) 
{
    return true;
}
