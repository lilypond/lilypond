/*
  request.cc -- implement Request

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "request.hh"
#include "debug.hh"

void
Request::do_print() const
{
}

bool
Request::equal_b (Request *r) const
{
  return r->do_equal_b (this) || this->do_equal_b (r) ;
}

bool
Request::do_equal_b (Request*) const
{
  return true;
}
  
