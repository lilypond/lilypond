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

IMPLEMENT_IS_TYPE_B1 (Staff_margin_engraver, Engraver);
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
  Item * it =  i.elem_l_->access_Item ();

  if (!it
      || script_p_ 
      || !it->is_type_b (Bar::static_name())
      || (i.origin_grav_l_arr_.size() != 1))
    return;

  String string = get_property ("instrument");
  String str = get_property ("instr");
  if (now_moment () > Moment (0))
    string = str;

  if (!string.length_i ())
    return;

  script_p_ = new Script;
  script_p_->axis_ = X_AXIS;
  
  Text_def *td_p =new Text_def;
  td_p->align_dir_ = LEFT;
  td_p->text_str_ = string;
  // huh?
  script_p_->dir_ = RIGHT;
  script_p_->specs_p_ = td_p;
  script_p_->breakable_b_ = true;

  
  Scalar pri = get_property ("marginBreakPriority");
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

