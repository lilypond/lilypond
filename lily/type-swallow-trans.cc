/*
  type-swallow-trans.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "type-swallow-trans.hh"
#include "musical-request.hh"

Type_swallow_translator::Type_swallow_translator ()
{
  type_ =0;
}

bool
Type_swallow_translator::do_try_music (Music*r)
{
  //  if (type_ && type_->before (typeid( *r))) // 
  //  return true;
  return false;			// ugh. FIXME.
}



DECLARE_REQUEST_SWALLOWER(Skip_req);

