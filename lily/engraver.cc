/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond musictypesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "musical-request.hh"
#include "engraver.hh"
#include "engraver-group.hh"
#include "debug.hh"

void
Engraver::fill_staff_info (Staff_info&)
{
  
}


void
Engraver::announce_element (Score_element_info i)
{
  i.origin_grav_l_arr_.push (this);
  daddy_grav_l()->announce_element (i);
}


void
Engraver::typeset_element (Score_element*p)
{
  daddy_grav_l()->typeset_element (p);
}

Paper_def*
Engraver::paper() const
{
  return (Paper_def*)output_def_l_;
}


Staff_info
Engraver::get_staff_info() const
{
  if (daddy_grav_l())
    return daddy_grav_l()->get_staff_info();
  Staff_info info;
  return info;
}


IMPLEMENT_IS_TYPE_B1(Engraver, Translator);


Engraver_group_engraver*
Engraver::daddy_grav_l () const
{
  return (daddy_trans_l_ )
       ? (Engraver_group_engraver*) daddy_trans_l_->access_Engraver  ()
       : 0;
}
