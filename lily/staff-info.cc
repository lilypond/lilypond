/*
  staff-info.cc -- implement Staff_info

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "proto.hh"
#include "plist.hh"
#include "staff-info.hh"
#include "score-column.hh"

Score*
Staff_info::score_l()
{
    return score_l_;
}

Staff_info::Staff_info()
{
    score_l_ =0;
    c0_position_i_l_ = 0;
    time_C_ = 0;
    rhythmic_C_ =0;
    break_allowed_b_ = 0;
}

Score_column*
Staff_info::musical_l()
{
    return musical_l_;
}

Score_column*
Staff_info::command_l()
{
    return command_l_;
}

PCol*
Staff_info::command_pcol_l()
{
    return command_l();
}

PCol*
Staff_info::musical_pcol_l()
{
    return musical_l();
}

Moment
Staff_info::when()
{
    return command_l()->when ();
}

