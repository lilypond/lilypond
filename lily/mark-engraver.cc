/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/
#include "command-request.hh"
#include "mark-engraver.hh"
#include "engraver-group-engraver.hh"
#include "text-item.hh"


ADD_THIS_TRANSLATOR (Mark_engraver);

Mark_engraver::Mark_engraver ()
{
  mark_req_l_ = 0;
  axis_ = Y_AXIS;
  type_ = "mark";
  visibility_lambda_ = ly_eval_str ("mark-visibility");
}

void
Mark_engraver::do_post_move_processing ()
{
  mark_req_l_ = 0;
}


bool
Mark_engraver::do_try_music (Music* r_l)
{
  if (Mark_req *mr = dynamic_cast <Mark_req *> (r_l))
    {
      
      if (mark_req_l_ && mr->equal_b (mark_req_l_))
	return true;
      if (mark_req_l_)
	return false;
      mark_req_l_ = mr;
      return true;
    }
  return false;
}

void
Mark_engraver::do_process_requests ()
{
  if (mark_req_l_)
    {
      create_items (mark_req_l_);

      String t = mark_req_l_->str_;
      text_p_->set_elt_property ("text",
				 ly_str02scm ( t.ch_C()));
      SCM st = ly_str02scm ((t.index_any_i ("0123456789")  >= 0 )
			    ? "mark" : "large");
      text_p_->set_elt_property ("style",  st);
    }
}

