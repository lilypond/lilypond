/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "main.hh"

Lyric_engraver::Lyric_engraver()
{
  lreq_l_ =0;
  lyric_item_p_ =0;
}

bool
Lyric_engraver::do_try_music (Music*r)
{
  if (Lyric_req * lr = dynamic_cast <Lyric_req *> (r))
    {
      lreq_l_ = lr;
      return true;
    }
  return false;
}

void
Lyric_engraver::do_process_requests()
{
  if (lreq_l_) 
    {
      Text_def *td_p = new Text_def;
      td_p->text_str_ = lreq_l_->text_str_;
      td_p->align_dir_ = LEFT;
      Scalar style = get_property ("textstyle");
      if (style.length_i ())
	{
	  td_p->style_str_ = style;
	}
      Scalar alignment = get_property ("textalignment");
      if (alignment.isnum_b())
	{
	  td_p->align_dir_= (Direction)(int)alignment;
	}
      
      lyric_item_p_ =  new Text_item (td_p);

      lyric_item_p_->dir_ = DOWN;
      lyric_item_p_->fat_b_ = true;
      announce_element (Score_element_info (lyric_item_p_, lreq_l_));
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



ADD_THIS_TRANSLATOR(Lyric_engraver);
