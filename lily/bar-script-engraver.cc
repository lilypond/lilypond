/*   
  bar-script-engraver.cc --  implement Bar_script_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "bar-script-engraver.hh"
#include "bar.hh"
#include "clef-item.hh"
#include "g-staff-side.hh"
#include "g-text-item.hh"
#include "lily-guile.hh"
#include "p-col.hh"

Bar_script_engraver::Bar_script_engraver ()
{
  axis_ = Y_AXIS;
  staff_side_p_ = 0;
  text_p_ =0;
  hang_on_clef_b_ = false;
  visibility_lambda_ 
    = gh_eval_str ("non_postbreak_visibility");
}

void
Bar_script_engraver::do_creation_processing ()
{
  Scalar prop = get_property (type_ + "HangOnClef", 0);
  if (prop.to_bool ())
    {
      hang_on_clef_b_ = true;
    }
}

void
Bar_script_engraver::do_acknowledge_element (Item *i)
{
  Axis other_axis = Axis((axis_ + 1)%2);
  if (staff_side_p_ && !staff_side_p_->dim_cache_[other_axis].parent_l_) 
    {
      staff_side_p_->dim_cache_[other_axis].parent_l_
	= &i->dim_cache_[other_axis];
      staff_side_p_->dim_cache_[axis_].parent_l_
	=  &i->dim_cache_[axis_];	  

      staff_side_p_->add_support (i);

      /*
	How do we make sure that staff_side_p_ has a dependency from
	someone else? We can't use i for that, 
       */
      get_staff_info ().command_pcol_l ()->add_dependency (staff_side_p_);
    }
}


Item*
Bar_script_engraver::cast_to_interesting_item (Score_element *e)
{
  Item * i =0;
  if (hang_on_clef_b_)
    {
      Clef_item * c = dynamic_cast<Clef_item*> (e);


      // urg.
      if (c) //  && c->default_b_)
	{
	  i = c;
	}
    }
  else
    {
      i = dynamic_cast<Bar*> (e);
    }
  return i;
}
					       
void
Bar_script_engraver::acknowledge_element (Score_element_info inf)
{
  if (inf.origin_grav_l_arr_.size () == 1)
    {
      Item *i=cast_to_interesting_item (inf.elem_l_);
      if (!i)
	return;

      /* Only put numbers on bars that are at our own level (don't put
	 numbers over the staffs of a GrandStaff, only over the GrandStaff
	 itself */
      if (inf.origin_grav_l_arr_.size () != 1)
	return;

      do_acknowledge_element (i);
    }
}

void 
Bar_script_engraver::do_pre_move_processing ()
{
  if (text_p_)
    {
      typeset_element (text_p_);
      text_p_ =0;
    }
  
  if (staff_side_p_) 
    {
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
  staff_side_p_->set_elt_property (breakable_scm_sym, SCM_BOOL_T); // ugh

  
  text_p_ = new G_text_item;
  text_p_->set_elt_property (breakable_scm_sym, SCM_BOOL_T); // ugh

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
      staff_side_p_->set_elt_property (padding_scm_sym, gh_double2scm(Real(padding)));
    }

  
  staff_side_p_->set_elt_property (visibility_lambda_scm_sym,
				   visibility_lambda_);
  text_p_->set_elt_property (visibility_lambda_scm_sym,
			     visibility_lambda_);
  
  announce_element (Score_element_info (text_p_, rq));
  announce_element (Score_element_info (staff_side_p_, rq));
}
