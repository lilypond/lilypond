/*
  request.cc -- implement Request

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "request.hh"
#include "warn.hh"

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
  : Music ()
{
}

Script_req::Script_req ()
{
  set_direction (CENTER);
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

  return (ly_dir_p (d)) ?  to_dir (d) : CENTER;
}

Direction
Span_req::get_span_dir () const
{
  SCM d = get_mus_property ("span-direction");

  return (ly_dir_p (d)) ?  to_dir (d) : CENTER;
}

void
Span_req::set_span_dir (Direction d)
{
  set_mus_property ("span-direction", gh_int2scm (d));
}

ADD_MUSIC(Request);
