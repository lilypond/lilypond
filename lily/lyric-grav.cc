/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"

Lyric_engraver::Lyric_engraver()
{
  lreq_l_ =0;
  lyric_item_p_ =0;
}

bool
Lyric_engraver::do_try_request (Request*r)
{
  Musical_req * m =r->musical();
  if (!m || ! m->lreq_l()) 
    return false;
  lreq_l_ = m->lreq_l();

  return true;
}

void
Lyric_engraver::do_process_requests()
{
  if (lreq_l_) 
    {
      lyric_item_p_ =  new Text_item (lreq_l_->tdef_p_);

      lyric_item_p_->translate_axis (paper()->note_width ()/2 , X_AXIS);
      lyric_item_p_->dir_ = DOWN;
      lyric_item_p_->fat_b_ = true;
      announce_element (Score_elem_info (lyric_item_p_, lreq_l_));
    }
}

void
Lyric_engraver::do_post_move_processing()
{
  lreq_l_ =0;
}

void
Lyric_engraver::do_pre_move_processing()
{
  if (lyric_item_p_)
    {
      typeset_element (lyric_item_p_);
      lyric_item_p_ =0;
    }
}


IMPLEMENT_IS_TYPE_B1(Lyric_engraver,Engraver);
ADD_THIS_TRANSLATOR(Lyric_engraver);
