/*
  bar-number-grav.cc -- implement Bar_number_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "bar-number-engraver.hh"
#include "script.hh"
#include "text-def.hh"
#include "paper-def.hh"
#include "command-request.hh"
#include "bar.hh"
#include "span-bar.hh"
#include "stem.hh"
#include "time-description.hh"

Bar_number_engraver::Bar_number_engraver()
{
  script_p_ =0;
}

void
Bar_number_engraver::acknowledge_element (Score_element_info i)
{

  Item *it=i.elem_l_->access_Item ();
  if (script_p_
      || !it || !it->is_type_b (Bar::static_name()))
      return;

  /* Only put numbers on bars that are at our own level (don't put
    numbers over the staffs of a GrandStaff, only over the GrandStaff
    itself */
  if (i.origin_grav_l_arr_.size() != 1)
    return;

  Time_description const * time = get_staff_info().time_C_;
  if (!time || time->cadenza_b_)
    return;
  
  script_p_ = new Script;
  Text_def *td_p = new Text_def;
  td_p->text_str_ = to_str (time->bars_i_);

  td_p->align_dir_ = LEFT;

  script_p_->dir_ = UP;
  script_p_->axis_ = Y_AXIS;
  script_p_->specs_p_ = td_p->clone ();
  script_p_->breakable_b_ = true;

  Scalar pri = get_property ("barNumberBreakPriority");
  if (pri.length_i () && pri.isnum_b ())
    {
      script_p_->break_priority_i_ = int (pri);
    }
  else
    script_p_->break_priority_i_ = it->break_priority_i_;

  Scalar padding = get_property ("barScriptPadding");
  if (padding.length_i() && padding.isnum_b ())
    {
      script_p_->padding_f_ = Real(padding);
    }

  announce_element (Score_element_info (script_p_,0));
}

void
Bar_number_engraver::do_pre_move_processing()
{
  if (script_p_) 
    {
      typeset_element (script_p_);
      script_p_ =0;
    }
}

IMPLEMENT_IS_TYPE_B1(Bar_number_engraver,Engraver);
ADD_THIS_TRANSLATOR(Bar_number_engraver);
