/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script-engraver.hh"
#include "script.hh"
#include "side-position-interface.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "staff-symbol.hh"
#include "rhythmic-head.hh"
#include "dimension-cache.hh"

Script_engraver::Script_engraver()
{
  do_post_move_processing();
}

bool
Script_engraver::do_try_music (Music *r_l)
{
  if (Articulation_req *mr = dynamic_cast <Articulation_req *> (r_l))
    {
      for (int i=0; i < script_req_l_arr_.size(); i++) 
	{
	  if (script_req_l_arr_[i]->equal_b (mr))
	    return true;
	}
      script_req_l_arr_.push (mr);
      return true;
    }
  return false;
}

void
Script_engraver::do_process_requests()
{
  for (int i=0; i < script_req_l_arr_.size(); i++)
    {
      Articulation_req* l=script_req_l_arr_[i];

      SCM list = ly_eval_str (("(articulation-to-scriptdef \"" + l->articulation_str_ + "\")").ch_C());
      
      if (list == SCM_BOOL_F)
	{
	  l->warning (_f ("Don't know how to interpret articulation `%s'",
			l->articulation_str_.ch_C ()));
	  continue;
	}
      Script *p =new Script;
      Side_position_interface stafy (p);
      
      
      list = gh_cdr (list);
      p->set_elt_property ("molecule",
			   SCM_CAR(list));

      list = SCM_CDR(list);
      bool follow_staff = gh_scm2bool (SCM_CAR(list));
      list = SCM_CDR(list);
      int relative_stem_dir = gh_scm2int (SCM_CAR(list));
      list = SCM_CDR(list);
      int force_dir =gh_scm2int (SCM_CAR(list));
      list = SCM_CDR(list);
      SCM priority = SCM_CAR(list);

      
      if (relative_stem_dir)
	  p->set_elt_property ("side-relative-direction", gh_int2scm (relative_stem_dir));
      else
	  stafy.set_direction ((Direction)force_dir);

      if (l->get_direction ())
	stafy.set_direction (l->get_direction ());

      SCM axisprop = get_property ("scriptHorizontal",0);
      bool xaxis = gh_boolean_p (axisprop) && gh_scm2bool (axisprop);
      if (xaxis)
	stafy.set_axis (X_AXIS);
      else
	stafy.set_axis (Y_AXIS);
      
      if (!follow_staff && ! xaxis)
	p->set_elt_property ("staff-support", SCM_BOOL_T);

      if (!xaxis && follow_staff)
	stafy.set_quantised (Y_AXIS);
      
      p->set_elt_property ("script-priority", priority);
  
      script_p_arr_.push (p);
      
      announce_element (Score_element_info (p, l));
    }
}

void
Script_engraver::acknowledge_element (Score_element_info inf)
{
  if (Stem *s = dynamic_cast<Stem*>(inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size(); i++)
	{
	  Side_position_interface stafy (script_p_arr_[i]);
	  stafy.elt_l_->set_elt_property ("direction-source", s->self_scm_);
	  stafy.add_support (s);
	}
    }
  else if (Rhythmic_head * rh = dynamic_cast<Rhythmic_head*>(inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size(); i++)
	{
	  Side_position_interface stafy(script_p_arr_[i]);
	  
	  if (!stafy.elt_l_->parent_l (X_AXIS))
	    {
	      stafy.elt_l_->set_parent (inf.elem_l_, X_AXIS);
	    }
	  if (stafy.get_axis () == X_AXIS
	      && !stafy.elt_l_->parent_l (Y_AXIS))
	    stafy.elt_l_->set_parent (rh, Y_AXIS);
	  
	  stafy.add_support (rh);
	}
    }  
}

void
Script_engraver::do_pre_move_processing()
{
  for (int i=0; i < script_p_arr_.size(); i++) 
    {
      typeset_element (script_p_arr_[i]);
    }
  script_p_arr_.clear();
}

void
Script_engraver::do_post_move_processing()
{
  script_req_l_arr_.clear();
}

ADD_THIS_TRANSLATOR(Script_engraver);


