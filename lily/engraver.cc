/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond music type setter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "grob.hh"
#include "score-engraver.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"
#include "context.hh"
#include "score-context.hh"

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
  /*
    TODO: junk grob-info, and make a cause grob-property to store
    `causes' generically.
  */
  if (unsmob_music (cause) || unsmob_grob (cause))
    e->set_property ("cause", cause);

  Grob_info i;
  i.grob_ = e;
  if (!i.origin_trans_)
    i.origin_trans_ = this;

  get_daddy_engraver ()->announce_grob (i);
}


 
void
Engraver::typeset_grob (Grob*p)
{
  Engraver *dad = get_daddy_engraver ();
  dad->typeset_grob (p);
}



Engraver::Engraver()
{
}


Score_engraver* 
Engraver::get_score_engraver () const
{
  SCM t = get_score_context ()->implementation_;
  return dynamic_cast<Score_engraver*> (unsmob_translator (t));
}


ENTER_DESCRIPTION(Engraver,
		   "", "",
		  "",
		  "", "", "");

