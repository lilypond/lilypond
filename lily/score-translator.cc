/*
  score-translator.cc -- implement Score_translator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score-translator.hh"
#include "moment.hh"

void
Score_translator::prepare (Moment)
{
}

SCM
Score_translator::get_output ()
{
  return SCM_EOL;
}

void
Score_translator::finish ()
{
}

void
Score_translator::one_time_step ()
{
}
