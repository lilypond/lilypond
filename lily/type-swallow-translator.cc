/*
  type-swallow-translator.cc -- implement Type_swallow_translator

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "type-swallow-translator.hh"
#include "musical-request.hh"


bool
Type_swallow_translator::try_music (Music*r)
{
  if (classname (r) == swallow_str_)
      return true;
  return false;
}

DECLARE_REQUEST_SWALLOWER(Skip_req);
