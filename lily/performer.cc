/*
  performer.cc -- implement Performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */


#include "performer-group-performer.hh"
#include "warn.hh"

void 
Performer::play_element (Audio_element* p) 
{ 
  get_daddy_perf ()->play_element (p); 
}

int
Performer::get_tempo () const
{
  return get_daddy_perf ()->get_tempo ();
}

Performer_group_performer*
Performer::get_daddy_perf () const
{
  return (daddy_trans_) 
    ?dynamic_cast<Performer_group_performer *> (daddy_trans_)
    : 0;
}

void
Performer::acknowledge_audio_element (Audio_element_info)
{
}

void
Performer::create_audio_elements ()
{
}


void
Performer::announce_element (Audio_element_info i)
{
  if (!i.origin_trans_)
    i.origin_trans_= this;
  get_daddy_perf ()->announce_element (i);
}
