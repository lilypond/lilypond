/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "paper-def.hh"
#include "main.hh"
#include "dimensions.hh"

ADD_THIS_TRANSLATOR(Lyric_engraver);

Lyric_engraver::Lyric_engraver()
{
}

bool
Lyric_engraver::do_try_music (Music*r)
{
  if (Lyric_req* l = dynamic_cast <Lyric_req *> (r))
    {
      lyric_req_l_arr_.push (l);
      return true;
    }
  return false;
}

void
Lyric_engraver::do_process_requests()
{
  if (text_p_arr_.size ())
    return;

  Scalar style = get_property ("textstyle");
  Scalar alignment = get_property ("textalignment");
  for (int i=0; i < lyric_req_l_arr_.size (); i++)
    {
      Lyric_req* request_l = lyric_req_l_arr_[i];
      Text_def* text_p = new Text_def;
      text_p->text_str_ = request_l->text_str_;
      text_p->align_dir_ = LEFT;
      if (style.length_i ())
	text_p->style_str_ = style;
      if (alignment.isnum_b())
	text_p->align_dir_= (Direction)(int)alignment;
      
      Text_item* item_p =  new Text_item (text_p);
      item_p->dir_ = DOWN;
      item_p->fat_b_ = true;
      // urg
      // item_p->translate (Offset (0, (i - 1) * item_p->height ().length_i ()));
//      if (i && ((Text_def*)text_p_arr_[i - 1]->tdef_p_)->text_str_.length_i ())
      // urg, when/how can one get the heigt of this thing?
      item_p->translate (Offset (0, - i * 12 PT));
      text_p_arr_.push (item_p);
      announce_element (Score_element_info (item_p, request_l));
    }
}

void
Lyric_engraver::do_post_move_processing()
{
}

void
Lyric_engraver::do_pre_move_processing()
{
  for (int i=0; i < text_p_arr_.size (); i++)
    {
      typeset_element (text_p_arr_[i]);
    }
  text_p_arr_.clear ();
  lyric_req_l_arr_.clear ();
}

