/*
  score-column.cc -- implement Score_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "debug.hh"
#include "p-col.hh"
#include "score-column.hh"
#include "command-request.hh"

Score_column::Score_column (Moment w, bool musical_b)
{
  break_penalty_i_ = 0;
  when_ = w;
  musical_b_ = musical_b;
}

void
Score_column::do_print() const
{
#ifndef NPRINT
  DOUT << "mus "<< musical_b_ << " at " <<  when_ << '\n';
  if (break_penalty_i_ >= Break_req::FORCE)
    DOUT << "Break forced";
      
  DOUT << "durations: [";
  for (int i=0; i < durations.size(); i++)
    DOUT << durations[i] << " ";
  DOUT << "]\n";
  Paper_column::do_print();
#endif
}

int
Moment_compare (Moment const &m1, Moment const &m2)
{
  return sign (m1-m2);
}

void
Score_column::preprocess()
{
  Paper_column ::preprocess ();
  durations.sort (Moment_compare);
}

void
Score_column::add_duration (Moment d)
{
  if (!d)
    {
      warning (_f ("ignoring zero duration added to column at %s",
	       when_.str ()));
      return;
    }
  
  for (int i = 0; i< durations.size(); i++) 
    {
      if (d == durations[i])
	return ;
    }
  durations.push (d);
}


