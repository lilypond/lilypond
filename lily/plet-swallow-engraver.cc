/*
  plet-swallow-engraver.cc -- implement Plet_swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "musical-request.hh"
#include "plet-swallow-engraver.hh"

IMPLEMENT_IS_TYPE_B1(Plet_swallow_engraver,Swallow_engraver);
ADD_THIS_TRANSLATOR(Plet_swallow_engraver);

bool
Plet_swallow_engraver::do_try_request (Request* req_l)
{
  if (req_l && req_l->musical () && req_l->musical ()->plet ())
    return true;
  return false;
}
