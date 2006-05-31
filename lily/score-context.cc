/*
  score-context.cc -- implement Score_context

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score-context.hh"

#include "score-translator.hh"

void
Score_context::prepare (Moment w)
{
  Translator_group *t = implementation ();
  Score_translator *s = dynamic_cast<Score_translator *> (t);

  s->prepare (w);
}

void
Score_context::finish ()
{
  Translator_group *t = implementation ();
  Score_translator *s = dynamic_cast<Score_translator *> (t);

  s->finish ();
}

void
Score_context::one_time_step ()
{
  Translator_group *t = implementation ();
  Score_translator *s = dynamic_cast<Score_translator *> (t);
  s->one_time_step ();
}

SCM
Score_context::get_output ()
{
  Translator_group *t = implementation ();
  Score_translator *s = dynamic_cast<Score_translator *> (t);
  return s->get_output ();
}

Score_context::Score_context (Object_key const *key)
  : Context (key)
{
}
