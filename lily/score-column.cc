/*
  score-column.cc -- implement Score_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "debug.hh"
#include "p-col.hh"
#include "score-column.hh"

Score_column::Score_column (Moment w)
{
  when_ = w;
  musical_b_ = false;
}

void
Score_column::print() const
{
#ifndef NPRINT
  DOUT << "Score_column { mus "<< musical_b_ <<" at " <<  when_<<'\n';
  DOUT << "durations: [";
  for (int i=0; i < durations.size(); i++)
	DOUT << durations[i] << " ";
  DOUT << "]\n";
  PCol::print();
  DOUT << "}\n";
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
  durations.sort (Moment_compare);
}

void
Score_column::add_duration (Moment d)
{
  assert (d);
  for (int i = 0; i< durations.size(); i++) 
    {
	if (d == durations[i])
	    return ;
    }
  durations.push (d);
}

void
Score_column::do_set_breakable()
{
  Score_column *c1 = new Score_column (when_);
  Score_column *c2 = new Score_column (when_);
  prebreak_p_ =c1;
  postbreak_p_ = c2;
  c1->durations = durations;
  c2->durations = durations;
  c1->musical_b_ 
	= c2->musical_b_ = musical_b_;
}
