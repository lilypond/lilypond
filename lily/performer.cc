/*
  performer.cc -- implement Performer

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
               Jan Nieuwenhuizen <janneke@gnu.org>
 */


#include "performer-group-performer.hh"
#include "debug.hh"


IMPLEMENT_IS_TYPE_B1(Performer, Translator);

void 
Performer::play (Audio_element* p) 
{ 
  daddy_perf_l ()->play (p); 
}

int
Performer::get_tempo_i() const
{
  return daddy_perf_l ()->get_tempo_i();
}

Performer_group_performer*
Performer::daddy_perf_l () const
{
  return (daddy_trans_l_) 
    ?(Performer_group_performer*) daddy_trans_l_->access_Performer ()
    : 0;
}
