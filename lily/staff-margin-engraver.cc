/*
  staff-margin-engraver.cc -- implement Staff_margin_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "staff-margin-engraver.hh"
#include "bar.hh"
#include "time-description.hh"
#include "g-text-item.hh"
#include "g-staff-side.hh"

ADD_THIS_TRANSLATOR (Staff_margin_engraver);

Staff_margin_engraver::Staff_margin_engraver ()
{
  axis_ = X_AXIS;
  type_ = "margin";
  visibility_lambda_
    = gh_eval_str ("postbreak_only_visibility");
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
  Bar * b =dynamic_cast<Bar *> (i.elem_l_);
  if (!b)
    return ;

  if (i.origin_grav_l_arr_.size() != 1)
    return;


  String long_str = get_property ("instrument", 0);
  String str = get_property ("instr", 0);
  if (now_mom () > Moment (0) && str.length_i ())
    long_str = str;

  if (long_str.empty_b ())
    return;

  create_items (0);
  text_p_->text_str_ = long_str;
  staff_side_p_->dir_ = LEFT;
  Bar_script_engraver::acknowledge_element(i);
}


