/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script-engraver.hh"
#include "script.hh"
#include "stem-staff-side.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "staff-symbol.hh"
#include "rhythmic-head.hh"

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


      SCM list = gh_eval_str (("(articulation-to-scriptdef \"" + l->articulation_str_ + "\")").ch_C());
      
      if (list == SCM_BOOL_F)
	{
	  l->warning(_f("don't know how to interpret articulation `%s'\n",
			l->articulation_str_.ch_C()));
	  continue;
	}
      Script *p =new Script;
      Stem_staff_side_item * ss =new Stem_staff_side_item;
      list = SCM_CDR (list);
	  
      p->set_elt_property (molecule_scm_sym,
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
	  ss->relative_dir_ = relative_stem_dir;
      else
	  ss->dir_ = force_dir;

      if (l->dir_)
	ss->dir_ = l->dir_;

      Real padding = 1.0;
      Scalar paddingprop = get_property ("articulationScriptPadding", 0);
      if (paddingprop.length_i() && paddingprop.isnum_b ())
	{
	  padding = (Real)paddingprop;
	}
      
      ss->staff_support_b_ = !follow_staff;
      p->set_staff_side (ss);
      ss->set_elt_property (script_priority_scm_sym, priority);
      ss->set_elt_property (padding_scm_sym, gh_double2scm(padding));
      script_p_arr_.push (p);
      staff_side_p_arr_.push (ss);
      
      announce_element (Score_element_info (p, l));
      announce_element (Score_element_info (ss, l));
    }
}

void
Script_engraver::acknowledge_element (Score_element_info inf)
{
  if (Stem *s = dynamic_cast<Stem*>(inf.elem_l_))
    {
      for (int i=0; i < staff_side_p_arr_.size(); i++)
	if (Stem_staff_side_item * ss = dynamic_cast<Stem_staff_side_item*>(staff_side_p_arr_[i]))
	  {
	    ss->set_stem (s);
	    ss->add_support (s);
	  }
    }
  else if (Rhythmic_head * rh = dynamic_cast<Rhythmic_head*>(inf.elem_l_))
    {
      for (int i=0; i < staff_side_p_arr_.size(); i++)
	{
	  Staff_side_item * ss = dynamic_cast<Staff_side_item*>(staff_side_p_arr_[i]);
	  
	  if (!ss->dim_cache_[X_AXIS]->parent_l_)
	    {
	      ss->dim_cache_[X_AXIS]->parent_l_ = inf.elem_l_->dim_cache_[X_AXIS];
	    }
	  ss->add_support (rh);
	}
    }  
}

void
Script_engraver::do_pre_move_processing()
{
  for (int i=0; i < script_p_arr_.size(); i++) 
    {
      typeset_element (script_p_arr_[i]);
      typeset_element (staff_side_p_arr_[i]);
    }
  script_p_arr_.clear();
  staff_side_p_arr_.clear ();
}

void
Script_engraver::do_post_move_processing()
{
  script_req_l_arr_.clear();
}



ADD_THIS_TRANSLATOR(Script_engraver);

