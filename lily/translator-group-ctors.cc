/*
  translator-group-ctors.cc -- implement Translator_group factory.

  source file of the GNU LilyPond music typesetter

  (c) 2005 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score-engraver.hh"
#include "score-performer.hh"
#include "engraver-group-engraver.hh"
#include "performer-group-performer.hh"
#include "recording-group-engraver.hh"

/*
  Quick & dirty.
*/
Translator_group *
get_translator_group (SCM sym)
{
  if (sym == ly_symbol2scm ("Engraver_group"))
    return new Engraver_group ();
  else if (sym == ly_symbol2scm ("Performer_group"))
    return new Performer_group ();
  else if (sym == ly_symbol2scm ("Score_engraver"))
    return new Score_engraver ();
  else if (sym == ly_symbol2scm ("Score_performer"))
    return new Score_performer ();
  else if (sym == ly_symbol2scm ("Recording_group_engraver"))
    return new Recording_group_engraver ();

  assert (0);
  return 0;
}
