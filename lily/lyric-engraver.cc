/*
  lyric-engraver.cc -- implement Lyric_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "lyric-engraver.hh"
#include "musical-request.hh"
#include "item.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "side-position-interface.hh"

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
Lyric_engraver::do_process_music()
{
  if (req_l_)
    {
      text_p_=  new Item (get_property ("LyricText"));
      
      text_p_->set_elt_property ("text", req_l_->get_mus_property ("text"));

      /*
	We can't reach the notehead where we're centered from here. So
	we kludge.

	(UGH UGH, pulled amount of space out of thin air)
      */
      
      text_p_->translate_axis (paper_l()->get_var ("staffspace")*0.66, X_AXIS);
      
      announce_element (text_p_, req_l_);
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


