/*
  request.cc -- implement Request

  source file of the GNU LilyPond music typesetter

  (c) 1996, 1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "request.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B1(Request,Music);

void
Request::do_print() const
{
}




bool
Request::equal_b (Request *r) const
{
  if (is_type_b (r->name()))
    return r->do_equal_b ((Request*)this);
  if (r->is_type_b (name()))
    return do_equal_b (r);
	 
  return false;
}

bool
Request::do_equal_b (Request*) const
{
  return false;
}
  
