/*
  plet-swallow-engraver.cc -- implement Plet_swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "plet-swallow-engraver.hh"

IMPLEMENT_IS_TYPE_B1(Plet_swallow_engraver,Swallow_engraver);
ADD_THIS_TRANSLATOR(Plet_swallow_engraver);

bool
Plet_swallow_engraver::do_try_request (Request* req_l)
{
  if (req_l && req_l->access_Musical_req () && req_l->access_Musical_req ()->access_Plet_req ())
    return true;
  return false;
}
