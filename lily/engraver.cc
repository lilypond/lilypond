/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond music type setter

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "grob.hh"
#include "score-engraver.hh"
#include "warn.hh"
#include "spanner.hh"
#include "item.hh"

void
Engraver::announce_grob (Grob_info inf)
{
  get_daddy_grav ()->announce_grob (inf);
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
    e->set_grob_property ("cause", cause);

  Grob_info i;
  i.grob_ = e;
  if (!i.origin_trans_)
    i.origin_trans_ = this;



  get_daddy_grav ()->announce_grob (i);
}


 
void
Engraver::typeset_grob (Grob*p)
{
  get_daddy_grav ()->typeset_grob (p);
}


Engraver_group_engraver*
Engraver::get_daddy_grav () const
{
  return (daddy_trans_)
       ? dynamic_cast<Engraver_group_engraver *> (daddy_trans_)
       : 0;
}

void
Engraver::process_music ()
{
  
}

Item*
Engraver::internal_make_item (SCM x)
{
  SCM props = internal_get_property (x);
  return new Item (props);
}

Spanner*
Engraver::internal_make_spanner (SCM x)
{
  SCM props = internal_get_property (x);
  return new Spanner (props);
}

Engraver::Engraver()
{
}


Score_engraver* 
Engraver::top_engraver () const
{
  return dynamic_cast<Score_engraver*> (top_translator());
}

ENTER_DESCRIPTION(Engraver,
		   "", "",
		  "",
		  "", "", "");

