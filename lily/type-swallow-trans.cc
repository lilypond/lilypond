/*
  type-swallow-trans.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "type-swallow-trans.hh"
#include "musical-request.hh"

Type_swallow_translator::Type_swallow_translator ()
{
  type_ =0;
}

bool
Type_swallow_translator::do_try_request (Request*r)
{
  if (type_&&r->is_type_b (type_))
      return true;
  return false;
}

IMPLEMENT_IS_TYPE_B1(Type_swallow_translator, Translator);

DECLARE_REQUEST_SWALLOWER(Skip_req);

