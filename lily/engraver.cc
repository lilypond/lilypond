/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond music type setter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "grob.hh"
#include "main.hh"


void
Engraver::announce_grob (Grob_info inf)
{
  daddy_grav_l ()->announce_grob (inf);
}

void
Engraver::announce_grob (Grob* e, Music *m)
{
  if (m && store_locations_global_b 
      && m->origin ()->location_str ().length_i ())
    {
      e->set_grob_property ("origin", m->get_mus_property ("origin"));
    }
  
  Grob_info i (e, m);
  if (!i.origin_trans_l_)
    i.origin_trans_l_ = this;
  daddy_grav_l ()->announce_grob (i);
}


 
void
Engraver::typeset_grob (Grob*p)
{
  daddy_grav_l ()->typeset_grob (p);
}


Engraver_group_engraver*
Engraver::daddy_grav_l () const
{
  return (daddy_trans_l_)
       ? dynamic_cast<Engraver_group_engraver *> (daddy_trans_l_)
       : 0;
}

void
Engraver::process_music ()
{
  
}
