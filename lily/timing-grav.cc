/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#if 0

#include "timing-grav.hh"
#include "command-request.hh"


Timing_engraver::Timing_engraver()
{
  default_grouping_ = Rhythmic_grouping (MInterval (0,4),4); // ugh
}

void
Timing_engraver::fill_staff_info (Staff_info &inf)
{
  inf.time_C_ = &time_;
  inf.rhythmic_C_ = &default_grouping_;
}


#endif
