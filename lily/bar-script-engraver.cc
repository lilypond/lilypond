/*   
  bar-script-engraver.cc --  implement Bar_script_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "bar-script-engraver.hh"
#include "bar.hh"
#include "clef-item.hh"
#include "staff-side.hh"
#include "text-item.hh"
#include "lily-guile.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "dimension-cache.hh"


Bar_script_engraver::Bar_script_engraver ()
{
  axis_ = Y_AXIS;
  staff_side_p_ = 0;
  text_p_ =0;
  hang_on_clef_b_ = false;
  visibility_lambda_ 
    = ly_eval_str ("non-postbreak-visibility");
}

void
Bar_script_engraver::do_creation_processing ()
{
  SCM prop = get_property (type_ + "HangOnClef", 0);
  if (gh_boolean_p (prop) && gh_scm2bool (prop))
    {
      hang_on_clef_b_ = true;
    }
}

/*
  Some interesting item came across.  Lets attach the text and the
  positioner to the item.

*/
 
void
Bar_script_engraver::attach_script_to_item (Item *i)
{
  Axis other_axis = Axis((axis_ + 1)%2);
  if (staff_side_p_ && !staff_side_p_->parent_l(other_axis)) 
    {
      staff_side_p_->set_parent (i,other_axis);
      staff_side_p_->set_parent (i,axis_);

      if (!text_p_->parent_l(other_axis))
	text_p_->set_parent (i,other_axis);
      staff_side_p_->add_support (i);

      /*
	How do we make sure that staff_side_p_ has a dependency from
	someone else? We can't use I for that,  so we use some other element.
       */
      // staff_side_p_->set_elt_property ("dangling", SCM_BOOL_T)
      get_staff_info ().command_pcol_l ()->add_dependency (staff_side_p_);
    }
}

/*
  URG.
 */
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
  if (inf.origin_trans_l_arr_.size () == 1)
    {
      Item *i=cast_to_interesting_item (inf.elem_l_);
      if (!i)
	return;

      /* Only put numbers on bars that are at our own level (don't put
	 numbers over the staffs of a GrandStaff, only over the GrandStaff
	 itself */
      if (inf.origin_trans_l_arr_.size () != 1)
	return;

      attach_script_to_item (i);
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
  
  staff_side_p_ = new Staff_side_item;
  staff_side_p_->axis_ = axis_;
  staff_side_p_->set_elt_property ("breakable", SCM_BOOL_T); // ugh

  
  text_p_ = new Text_item;
  text_p_->set_elt_property ("breakable", SCM_BOOL_T); // ugh

  SCM prop = get_property (type_ + "Direction", 0);
  if (isdir_b (prop))
    {
      staff_side_p_->set_direction ( to_dir (prop));
    }
  else 
    {
      staff_side_p_->set_direction ( UP);
    }

  staff_side_p_->set_victim(text_p_);
  
  SCM padding = get_property (type_ + "ScriptPadding", 0);
  if (gh_number_p(padding))
    {
      staff_side_p_->set_elt_property ("padding", padding);
    }
  else
    {
      staff_side_p_
	->set_elt_property ("padding",
			    gh_double2scm(paper_l ()->get_var ("interline")));
    }
  
  staff_side_p_->set_elt_property ("visibility-lambda",
				   visibility_lambda_);
  text_p_->set_elt_property ("visibility-lambda",
			     visibility_lambda_);
  
  announce_element (Score_element_info (text_p_, rq));
  announce_element (Score_element_info (staff_side_p_, rq));
}

