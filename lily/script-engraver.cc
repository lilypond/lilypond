/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "script.hh"
#include "side-position-interface.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"

class Script_engraver : public Engraver {
  Link_array<Score_element> script_p_arr_;
  Link_array<Articulation_req> script_req_l_arr_;

public:
  VIRTUAL_COPY_CONS(Translator);
  
  Script_engraver();
protected:
  virtual bool do_try_music (Music*);
  virtual void do_process_music ();
  virtual void do_pre_move_processing ();
  virtual void do_post_move_processing ();
  virtual void acknowledge_element (Score_element_info);
};


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
Script_engraver::do_process_music()
{
  for (int i=0; i < script_req_l_arr_.size(); i++)
    {
      Articulation_req* l=script_req_l_arr_[i];

      SCM alist = get_property ("scriptDefinitions");
      SCM list = scm_assoc (l->get_mus_property ("articulation-type"), alist);

      if (list == SCM_BOOL_F)
	{
	  String a = ly_scm2string (l->get_mus_property ("articulation-type"));
	  l->origin ()->warning (_f ("Don't know how to interpret articulation `%s'", a.ch_C()));
			
	  continue;
	}
      // todo -> use result of articulation-to-scriptdef directly as basic prop list.
      Score_element *p =new Item (get_property ("Script"));
      list = gh_cdr (list);
      p->set_elt_property ("molecule",
			   gh_car (list));

      list = gh_cdr(list);
      bool follow_staff = gh_scm2bool (gh_car (list));
      list = gh_cdr(list);
      SCM relative_stem_dir = gh_car (list);
      list = gh_cdr(list);

      SCM force_dir = l->get_mus_property ("direction");
      if (!isdir_b (force_dir))
	force_dir = gh_car (list);
      list = gh_cdr(list);
      SCM priority = gh_car (list);

      
      if (!isdir_b (force_dir)
	  && to_dir (relative_stem_dir))
	p->set_elt_property ("side-relative-direction", relative_stem_dir);
      else
	p->set_elt_property ("direction", force_dir);

      /*
	FIXME: should figure this out in relation with basic props! 
       */
      SCM axisprop = get_property ("scriptHorizontal");
      bool xaxis = to_boolean (axisprop);
      Side_position::set_axis (p, xaxis ? X_AXIS : Y_AXIS);
      
      if (!follow_staff && ! xaxis)
	p->set_elt_property ("staff-support", SCM_BOOL_T);

      if (!xaxis && follow_staff)
	p->add_offset_callback (Side_position::quantised_position_proc, Y_AXIS);
      
      
      p->set_elt_property ("script-priority", priority);
  
      script_p_arr_.push (p);
      
      announce_element (p, l);
    }
}

void
Script_engraver::acknowledge_element (Score_element_info inf)
{
  bool them_grace = to_boolean (inf.elem_l_->get_elt_property ("grace"));
  bool us_grace = to_boolean (get_property ("weAreGraceContext"));

  if (us_grace != them_grace)
    return;
  
  if (Stem::has_interface (inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size(); i++)
	{
	  Score_element*e = script_p_arr_[i];

	  e->set_elt_property ("direction-source", inf.elem_l_->self_scm ());
	  Side_position::add_support (e, inf.elem_l_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size(); i++)
	{
	  Score_element *e = script_p_arr_[i];
	  
	  if (!e->parent_l (X_AXIS))
	    {
	      e->set_parent (inf.elem_l_, X_AXIS);
	    }
	  if (Side_position::get_axis (e) == X_AXIS
	      && !e->parent_l (Y_AXIS))
	    e->set_parent (inf.elem_l_, Y_AXIS);
	  
	  Side_position::add_support (e,inf.elem_l_);
	}
    }  
}

void
Script_engraver::do_pre_move_processing()
{
  for (int i=0; i < script_p_arr_.size(); i++) 
    {
      Score_element * sc = script_p_arr_[i];
      if (to_boolean (sc->get_elt_property ("staff-support")))
	{
	  Side_position::add_staff_support (sc);
	}
      typeset_element (sc);
    }
  script_p_arr_.clear();
}

void
Script_engraver::do_post_move_processing()
{
  script_req_l_arr_.clear();
}

ADD_THIS_TRANSLATOR(Script_engraver);


