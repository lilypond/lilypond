/*
  performer.cc -- implement Performer

  source file of the GNU LilyPond music typesetter

  (c) 1996--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */


#include "performer-group-performer.hh"
#include "debug.hh"

void 
Performer::play_element (Audio_element* p) 
{ 
  daddy_perf_l ()->play_element (p); 
}

int
Performer::get_tempo_i () const
{
  return daddy_perf_l ()->get_tempo_i ();
}

Performer_group_performer*
Performer::daddy_perf_l () const
{
  return (daddy_trans_l_) 
    ?dynamic_cast<Performer_group_performer *> (daddy_trans_l_)
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
  if (!i.origin_trans_l_)
    i.origin_trans_l_= this;
  daddy_perf_l ()->announce_element (i);
}
