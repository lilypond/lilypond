/*
  plet-swallow-engraver.cc -- implement Plet_swallow_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "musical-request.hh"
#include "plet-swallow-engraver.hh"


ADD_THIS_TRANSLATOR(Plet_swallow_engraver);

bool
Plet_swallow_engraver::do_try_music (Music* req_l)
{
  return dynamic_cast<Plet_req *> (req_l);
}
