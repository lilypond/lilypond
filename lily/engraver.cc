/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond music type setter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"

#include "music.hh"
#include "score-engraver.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"
#include "context.hh"
#include "score-context.hh"
#include "lilypond-key.hh"

Engraver_group_engraver*
Engraver::get_daddy_engraver () const
{
  return dynamic_cast<Engraver_group_engraver*> (get_daddy_translator ());
}

void
Engraver::announce_grob (Grob_info inf)
{
  get_daddy_engraver ()->announce_grob (inf);
}


/*
  CAUSE is the object (typically a Music object)  that
  was the reason for making E.
 */
void
Engraver::announce_grob (Grob* e, SCM cause)
{
  if (unsmob_music (cause) || unsmob_grob (cause))
    e->set_property ("cause", cause);

  Grob_info i;
  i.grob_ = e;
  if (!i.origin_trans_)
    i.origin_trans_ = this;

  Engraver * g = get_daddy_engraver ();
  if (g)
    g->announce_grob (i);
}


 


Engraver::Engraver ()
{
}


Score_engraver* 
Engraver::get_score_engraver () const
{
  return dynamic_cast<Score_engraver*> (get_score_context ()->implementation ());
}


ADD_TRANSLATOR (Engraver,
		   "", "",
		  "",
		  "", "", "");

