/*
  engraver.cc -- implement Engraver

  Sourcefile of GNU LilyPond musictypesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "music-list.hh"
#include "musical-request.hh"
#include "engraver.hh"
#include "engraver-group-engraver.hh"
#include "debug.hh"
#include "paper-def.hh"
#include "score-element.hh"

void
Engraver::fill_staff_info (Staff_info&)
{
  
}

void
Engraver::announce_element (Score_element_info i)
{
  i.origin_trans_l_arr_.push (this);
  daddy_grav_l()->announce_element (i);
}

void
Engraver::typeset_element (Score_element*p)
{
  daddy_grav_l()->typeset_element (p);
}


Paper_def*
Engraver::paper_l () const
{
  return dynamic_cast<Paper_def*>(output_def_l_);
}


Staff_info
Engraver::get_staff_info() const
{
  if (daddy_grav_l())
    return daddy_grav_l()->get_staff_info();
  Staff_info info;
  return info;
}





Engraver_group_engraver*
Engraver::daddy_grav_l () const
{
  return (daddy_trans_l_ )
       ? dynamic_cast<Engraver_group_engraver *> (daddy_trans_l_)
       : 0;
}
