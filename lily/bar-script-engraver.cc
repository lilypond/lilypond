/*   
  bar-script-engraver.cc --  implement Bar_script_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "bar-script-engraver.hh"
#include "bar.hh"
#include "g-staff-side.hh"
#include "g-text-item.hh"
#include "lily-guile.hh"

Bar_script_engraver::Bar_script_engraver ()
{
  axis_ = Y_AXIS;
  staff_side_p_ = 0;
  text_p_ =0;
  visibility_lambda_ 
    = gh_eval_str ("non_postbreak_visibility");
}

void
Bar_script_engraver::acknowledge_element (Score_element_info i)
{
  Axis other_axis = Axis((axis_ + 1)%2);
  
  if (staff_side_p_ && !staff_side_p_->dim_cache_[other_axis].parent_l_) 
    {
      Bar * bar_l = dynamic_cast<Bar*> (i.elem_l_);
      if (!bar_l)
	return;
      
      /* Only put numbers on bars that are at our own level (don't put
	 numbers over the staffs of a GrandStaff, only over the GrandStaff
	 itself */
      if (i.origin_grav_l_arr_.size () == 1)
	{
	  staff_side_p_->dim_cache_[other_axis].parent_l_ =  &bar_l->dim_cache_[other_axis];
	  //	  staff_side_p_->dim_cache_[axis_].parent_l_ =  &bar_l->dim_cache_[axis_];	  
	  staff_side_p_->add_support (i.elem_l_);
	  bar_l->add_dependency (staff_side_p_); // UGH. 
	}
    }
}

void 
Bar_script_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      text_p_->breakable_b_ = true; // ugh
      typeset_element (text_p_);
      text_p_ =0;
    }
  
  if (staff_side_p_) 
    {
      staff_side_p_->breakable_b_ = true; // ugh
      typeset_element (staff_side_p_);
      staff_side_p_ = 0;
    }
}


void
Bar_script_engraver::create_items (Request *rq)
{
  if (staff_side_p_ || text_p_)
    return;
  
  staff_side_p_ = new G_staff_side_item;
  staff_side_p_->axis_ = axis_;
  
  text_p_ = new G_text_item;

  Scalar prop = get_property (type_ + "Direction", 0);
  if (prop.isnum_b ())
    {
      staff_side_p_->dir_ = (Direction) (int) prop;
    }
  else 
    {
      staff_side_p_->dir_ = UP;
    }

  staff_side_p_->set_victim(text_p_);
  
  Scalar padding = get_property (type_ + "ScriptPadding", 0);
  if (padding.length_i() && padding.isnum_b ())
    {
      staff_side_p_->padding_f_ = Real(padding);
    }

  staff_side_p_->visibility_lambda_  = visibility_lambda_;
  text_p_->visibility_lambda_ = visibility_lambda_;  
  
  announce_element (Score_element_info (text_p_, rq));
  announce_element (Score_element_info (staff_side_p_, rq));
}
