/*
  request.cc -- implement Request

  source file of the GNU LilyPond music typesetter

  (c) 1996,1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "request.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B1(Request,Music);

void
Request::do_print() const
{
}

MInterval
Request::time_int() const
{
    return MInterval(0, duration());
}

