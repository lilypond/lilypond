/*
  break-align-item.cc -- implement Break_align_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "break-align-item.hh"
#include "dimensions.hh"

void
Break_align_item::do_pre_processing()
{
  align_dir_ = break_status_dir();
  flip (&align_dir_);
  Axis_align_item::do_pre_processing();
}

Break_align_item::Break_align_item ()
{
  stacking_dir_ = RIGHT;
  threshold_interval_[SMALLER] = 1.5 PT;
  set_axis (X_AXIS);
}
