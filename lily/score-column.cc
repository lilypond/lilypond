/*
  score-column.cc -- implement Score_column

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "p-col.hh"
#include "score-column.hh"

Score_column::Score_column (Moment w)
{
  forced_break_b_ = false;
  when_ = w;
  musical_b_ = false;
}

void
Score_column::do_print() const
{
#ifndef NPRINT
  DOUT << "mus "<< musical_b_ <<" at " <<  when_<< '\n';
  if (forced_break_b_)
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
      warning (_("Ignoring zero duration added to column at ")
	       + when_.str ());
      return;
    }
  
  for (int i = 0; i< durations.size(); i++) 
    {
      if (d == durations[i])
	return ;
    }
  durations.push (d);
}

IMPLEMENT_IS_TYPE_B1(Score_column, Paper_column);
