/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "text-item.hh"
#include "paper-def.hh"
#include "lookup.hh"

ADD_THIS_TRANSLATOR (Lyric_engraver);


Lyric_engraver::Lyric_engraver()
{
  text_p_ =0;
  req_l_ =0;
}

bool
Lyric_engraver::do_try_music (Music*r)
{
  if (Lyric_req* l = dynamic_cast <Lyric_req *> (r))
    {
      if (req_l_)
	return false;
      req_l_ =l;
      return true;
    }
  return false;
}

void
Lyric_engraver::do_process_requests()
{
  if (req_l_)
    {
      text_p_=  new Text_item;
      text_p_->text_str_ = req_l_->text_str_;
      text_p_->text_str_ += " ";	// ugh.

      text_p_->set_elt_property ("non-rhythmic", SCM_BOOL_T);
      
      announce_element (Score_element_info (text_p_, req_l_));
    }
}

void
Lyric_engraver::do_pre_move_processing()
{
  if (text_p_)
    {
      typeset_element (text_p_);
      text_p_ =0;
    }
}

void
Lyric_engraver::do_post_move_processing ()
{
  req_l_ =0;
}


