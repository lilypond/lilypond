/*
  timing-grav.cc -- implement Timing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "score-grav.hh"
#include "timing-grav.hh"
#include "command-request.hh"
#include "score-elem-info.hh"
#include "multi-measure-rest.hh"

void
Timing_engraver::acknowledge_element (Score_elem_info i)
{
  if (!i.elem_l_->is_type_b (Multi_measure_rest::static_name ()))
    return;
  if (((Multi_measure_rest*)i.elem_l_->item ())->measures_i_)
    mmrest_b_ = true;
}

void
Timing_engraver::fill_staff_info (Staff_info &inf)
{
  inf.time_C_ = &time_;
  inf.rhythmic_C_ = &default_grouping_;
}

IMPLEMENT_IS_TYPE_B1(Timing_engraver, Timing_translator);
ADD_THIS_TRANSLATOR(Timing_engraver);
