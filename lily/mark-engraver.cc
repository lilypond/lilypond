/*
  mark-engraver.cc -- implement Mark_engraver

  source file of the GNU LilyPond music typesetter

 (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "mark-engraver.hh"
#include "text-def.hh"
#include "script.hh"
#include "paper-def.hh"
#include "command-request.hh"
#include "time-description.hh"
#include "engraver-group.hh"

IMPLEMENT_IS_TYPE_B1 (Mark_engraver, Engraver);
ADD_THIS_TRANSLATOR (Mark_engraver);

Mark_engraver::Mark_engraver ()
{
  mark_req_l_ = 0;
  script_p_ = 0;
}

bool
Mark_engraver::do_try_request (Request* r_l)
{
  Command_req* c_l = r_l->access_Command_req ();
  if (!c_l || !c_l->access_Mark_req () || mark_req_l_) 
    return false;

  mark_req_l_ = c_l->access_Mark_req ();

  return true;
}

void
Mark_engraver::do_process_requests ()
{  
  if (!mark_req_l_ || script_p_)
    return;

  script_p_ = new Script;
  script_p_->breakable_b_ = true;

  Text_def *td_p = new Text_def;

  td_p->text_str_ = mark_req_l_->str_;
  td_p->align_dir_ = CENTER;

  td_p->style_str_ = td_p->text_str_.index_any_i ("0123456789") >= 0 
    ? "mark" : "Large";

  script_p_->dir_ = LEFT;
  script_p_->specs_p_ = td_p->clone ();
  script_p_->postbreak_only_b_ = true;
  
  Scalar padding = get_property ("markScriptPadding");
  if (padding.length_i() && padding.isnum_b ())
    {
      script_p_->padding_f_ = Real(padding);
    }
  Scalar break_priority = get_property ("markBreakPriority");
  if (break_priority.length_i() && break_priority.isnum_b ())
    {
      script_p_->break_priority_i_ = int(break_priority);
    }

  
  announce_element (Score_element_info (script_p_, mark_req_l_));
}

void 
Mark_engraver::do_pre_move_processing ()
{
  if (script_p_) 
    {
      typeset_element (script_p_);
      script_p_ = 0;
      mark_req_l_ = 0;
    }
}

