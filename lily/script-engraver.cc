/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script-engraver.hh"
#include "script.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "staff-sym.hh"
#include "general-script-def.hh"
#include "text-def.hh"


Script_engraver::Script_engraver()
{
  do_post_move_processing();
}

bool
Script_engraver::do_try_music (Music *r_l)
{
  if (Musical_script_req *mr = dynamic_cast <Musical_script_req *> (r_l))
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
  if (script_p_arr_.size ())
    return ;
  
  for (int i=0; i < script_req_l_arr_.size(); i++)
    {
      Script_req* l=script_req_l_arr_[i];
      Script *p =new Script;
      p->dir_ = l->dir_;
      p->specs_p_ = l->scriptdef_p_->clone ();
      script_p_arr_.push (p);
      announce_element (Score_element_info (p, l));
    }
}

void
Script_engraver::do_pre_move_processing()
{
  Staff_symbol* s_l = get_staff_info().staff_sym_l_;
  for (int i=0; i < script_p_arr_.size(); i++) 
    {
      Script*script_p = script_p_arr_[i];
      if (!script_p->specs_p_->inside_b())
	script_p->add_support (s_l);

      if (dynamic_cast<Text_def *> (script_p->specs_p_)) // UGH
	{
	  Text_def * td_l = (Text_def*)script_p->specs_p_;
	  if (!td_l->style_str_.length_i ())
	    {
	      Scalar style = get_property ("textstyle", 0);
	      if (style.to_bool ())
		td_l->style_str_= style;
	    }
	  // urg, what if this is already set? in-band signaling...
	  Scalar alignment = get_property ("textalignment", 0);
	  if (alignment.isnum_b())
	    {
	      td_l->align_dir_= (Direction)(int)alignment;
	    }
	}
      typeset_element (script_p);
    }
  script_p_arr_.clear();
}

void
Script_engraver::do_post_move_processing()
{
  script_req_l_arr_.clear();
}



ADD_THIS_TRANSLATOR(Script_engraver);
