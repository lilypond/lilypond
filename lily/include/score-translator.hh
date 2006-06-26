/*
  score-translator.hh -- declare Score_translator

  source file of the GNU LilyPond music typesetter

  (c) 2004--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SCORE_TRANSLATOR_HH
#define SCORE_TRANSLATOR_HH

#include "translator-group.hh"

class Score_translator : public virtual Translator_group
{
  friend class Score_context;
protected:
  virtual SCM get_output ();
};

#endif /* SCORE_TRANSLATOR_HH */

