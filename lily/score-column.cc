/*
  score-column.cc -- implement Score_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "paper-column.hh"
#include "score-column.hh"
#include "command-request.hh"

Score_column::Score_column (Moment w)
{
  break_penalty_i_ = 0;
  when_ = w;
}

void
Score_column::do_print() const
{
#ifndef NPRINT
  DOUT << " at " <<  when_ << '\n';
  if (break_penalty_i_ >= Break_req::FORCE)
    DOUT << "Break forced";

  DOUT << "Shortest playing: " <<  shortest_playing_mom_ << " shortest starter: " << shortest_starter_mom_;
  Paper_column::do_print();
#endif
}


bool
Score_column::musical_b () const
{
  return shortest_starter_mom_ != Moment(0);
}
