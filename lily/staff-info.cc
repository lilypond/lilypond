/*
  staff-info.cc -- implement Staff_info

  source file of the LilyPond music typesetter
  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
#include "proto.hh"
#include "plist.hh"
#include "staff.hh"
#include "staff-info.hh"
#include "score-column.hh"
#include "complex-walker.hh"
#include "staff-column.hh"

Staff*
Staff_info::staff_l()
{
    return walk_l_->staff_l_;
}

Score*
Staff_info::score_l()
{
    return staff_l()->score_l_;
}
Staff_info::Staff_info()
{
    c0_position_i_l_ = 0;
    walk_l_ = 0;
    time_C_ = 0;
    rhythmic_C_ =0;
    break_allowed_b_ = 0;
}

Staff_column*
Staff_info::column_l()
{
    return  walk_l_->ptr();
}

Score_column*
Staff_info::musical_l()
{
    return column_l() -> musical_column_l_;
}

Score_column*
Staff_info::command_l()
{
    return column_l() -> command_column_l_;
}
PCol*
Staff_info::command_pcol_l()
{
    return command_l()->pcol_l_;
}

Moment
Staff_info::when()
{
    return walk_l_->when();
}

