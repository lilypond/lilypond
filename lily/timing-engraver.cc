/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score-engraver.hh"
#include "timing-engraver.hh"
#include "command-request.hh"
#include "score-element-info.hh"
#include "multi-measure-rest.hh"

void
Timing_engraver::fill_staff_info (Staff_info &inf)
{
  inf.time_C_ = &time_;
}


ADD_THIS_TRANSLATOR(Timing_engraver);
