/*
  request.cc -- implement Request

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "request.hh"
#include "debug.hh"



bool
Request::equal_b (Request const* r) const
{
  return r->do_equal_b (this) || this->do_equal_b (r) ;
}

bool
Request::do_equal_b (Request const*) const
{
  return true;
}
  

Request::Request ()
{
  set_mus_property ("type", ly_symbol2scm ("request"));
}

void
Script_req::set_direction (Direction d)
{
  set_mus_property ("direction", gh_int2scm (d));
}

Direction
Script_req::get_direction () const
{
  SCM d = get_mus_property ("direction");

  return (isdir_b (d)) ?  to_dir (d) : CENTER;
}


