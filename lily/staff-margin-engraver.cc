/*
  staff-margin-engraver.cc -- implement Staff_margin_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "staff-margin-engraver.hh"
#include "script.hh"
#include "text-def.hh"
#include "paper-def.hh"
#include "command-request.hh"
#include "bar.hh"
#include "stem.hh"
#include "time-description.hh"


ADD_THIS_TRANSLATOR (Staff_margin_engraver);

Staff_margin_engraver::Staff_margin_engraver ()
{
  script_p_ = 0;
}



/*
    TODO
    fix alignment/support

    should be able to set whole paragraph (multiple lines, centre) to
    left (right?) of staff, e.g.:
                    ______
                   |_______
      2 Clarinetti |________
         (Bb)      |___________
	           |______________
*/
void
Staff_margin_engraver::acknowledge_element (Score_element_info i)
{
  Item * it =  dynamic_cast <Item *> (i.elem_l_);

  if (!it
      || script_p_ 
      || !(dynamic_cast<Bar *> (it))
      || (i.origin_grav_l_arr_.size() != 1))
    return;

  String string = get_property ("instrument", 0);
  String str = get_property ("instr", 0);
  if (now_moment () > Moment (0))
    string = str;

  if (!string.length_i ())
    return;

  script_p_ = new Script;
  script_p_->axis_ = X_AXIS;
  
  Text_def *td_p =new Text_def;
  td_p->align_dir_ = LEFT;
  td_p->text_str_ = string;
  script_p_->dir_ = LEFT;
  script_p_->specs_p_ = td_p;
  script_p_->breakable_b_ = true;

  
  Scalar pri = get_property ("marginBreakPriority", 0);
  if (pri.length_i () && pri.isnum_b ())
    {
      script_p_->break_priority_i_ = int (pri);
    }
  else
    script_p_ ->break_priority_i_ = it->break_priority_i_;

  announce_element (Score_element_info (script_p_, 0));
}

void
Staff_margin_engraver::do_pre_move_processing ()
{
  if (script_p_) 
    {
      typeset_element (script_p_);
      script_p_ =0;
    }
}

