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
#include "staff-sym.hh"
#include "g-text-item.hh"
#include "g-staff-side.hh"
#include "stem.hh"
#include "rhythmic-head.hh"

ADD_THIS_TRANSLATOR (Mark_engraver);

Mark_engraver::Mark_engraver ()
{
  mark_req_l_ = 0;
  staff_side_p_ = 0;
  text_p_ = 0;
}

bool
Mark_engraver::do_try_music (Music* r_l)
{
  if (Mark_req *mr = dynamic_cast <Mark_req *> (r_l))
    {
      mark_req_l_ = mr;
      return true;
    }
  return false;
}

void
Mark_engraver::do_process_requests ()
{  
  if (!mark_req_l_ || staff_side_p_)
    return;

  staff_side_p_ = new G_staff_side_item;

  text_p_ = new G_text_item;

  text_p_->text_str_ = mark_req_l_->str_;
  //  text_p_->align_dir_ = CENTER;

  text_p_->style_str_ = text_p_->text_str_.index_any_i ("0123456789") >= 0 
    ? "mark" : "Large";

  Scalar prop = get_property ("markdir", 0);
  if (prop.isnum_b ())
    {
      staff_side_p_->dir_ = (Direction) (int) prop;
    }
  else 
    {
      staff_side_p_->dir_ = UP;
    }

  staff_side_p_->set_victim(text_p_);
  
  //  Scalar padding = get_property ("markScriptPadding", 0);
  //  if (padding.length_i() && padding.isnum_b ())
  //    {
  //      script_p_->padding_f_ = Real(padding);
  //    }
  //  Scalar break_priority = get_property ("markBreakPriority", 0);
  //  if (break_priority.length_i() && break_priority.isnum_b ())
  //    {
  //      staff_side_p_->break_priority_i_ = int(break_priority);
  //    }

  
  announce_element (Score_element_info (text_p_, mark_req_l_));
  announce_element (Score_element_info (staff_side_p_, mark_req_l_));
}

void 
Mark_engraver::do_pre_move_processing ()
{
  if (staff_side_p_) 
    {
      Staff_symbol* s_l = get_staff_info().staff_sym_l_;
      staff_side_p_->add_support (s_l);
      typeset_element (text_p_);
      typeset_element (staff_side_p_);
      text_p_ = 0;
      staff_side_p_ = 0;
      mark_req_l_ = 0;
    }
}

void
Mark_engraver::acknowledge_element (Score_element_info i)
{
  if (staff_side_p_) 
    {
      if (dynamic_cast<Stem *> (i.elem_l_) ||
	  dynamic_cast<Rhythmic_head *> (i.elem_l_))
	{
	  staff_side_p_->add_support (i.elem_l_);
	}
    }
}
