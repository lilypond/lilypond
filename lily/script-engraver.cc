/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script.hh"
#include "side-position-interface.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"

class Script_engraver : public Engraver
{
  Link_array<Grob> script_p_arr_;
  Link_array<Articulation_req> script_req_l_arr_;

public:
  VIRTUAL_COPY_CONS (Translator);

protected:
  virtual bool try_music (Music*);
  virtual void initialize ();
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
};

void
Script_engraver::initialize ()
{
  script_req_l_arr_.clear ();
}

bool
Script_engraver::try_music (Music *r_l)
{
  if (Articulation_req *mr = dynamic_cast <Articulation_req *> (r_l))
    {
      for (int i=0; i < script_req_l_arr_.size (); i++) 
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
Script_engraver::process_music ()
{
  for (int i=0; i < script_req_l_arr_.size (); i++)
    {
      Articulation_req* l=script_req_l_arr_[i];

      SCM alist = get_property ("scriptDefinitions");
      SCM art = scm_assoc (l->get_mus_property ("articulation-type"), alist);

      if (art == SCM_BOOL_F)
	{
	  String a = ly_scm2string (l->get_mus_property ("articulation-type"));
	  l->origin ()->warning (_f ("Don't know how to interpret articulation `%s'", a.ch_C ()));
			
	  continue;
	}
      // todo -> use result of articulation-to-scriptdef directly as basic prop list.
      Grob *p =new Item (get_property ("Script"));
      art = ly_cdr (art);
      p->set_grob_property ("molecule", ly_car (art));

      art = ly_cdr (art);
      bool follow_staff = gh_scm2bool (ly_car (art));
      art = ly_cdr (art);
      SCM relative_stem_dir = ly_car (art);
      art = ly_cdr (art);

      SCM force_dir = l->get_mus_property ("direction");
      if (isdir_b (force_dir) && !to_dir (force_dir))
	force_dir = ly_car (art);
      
      art = ly_cdr (art);
      SCM priority = ly_car (art);

      if (isdir_b (force_dir) && to_dir (force_dir))
	p->set_grob_property ("direction", force_dir);
      else if (to_dir (relative_stem_dir))
	p->set_grob_property ("side-relative-direction", relative_stem_dir);


      /*
	FIXME: should figure this out in relation with basic props! 
       */
      SCM axisprop = get_property ("scriptHorizontal");
      bool xaxis = to_boolean (axisprop);
      Side_position_interface::set_axis (p, xaxis ? X_AXIS : Y_AXIS);
      
      if (!follow_staff && ! xaxis)
	p->set_grob_property ("staff-support", SCM_BOOL_T);

      if (!xaxis && follow_staff)
	p->add_offset_callback (Side_position_interface::quantised_position_proc, Y_AXIS);
      
      
      p->set_grob_property ("script-priority", priority);
  
      script_p_arr_.push (p);
      
      announce_grob (p, l);
    }
}

void
Script_engraver::acknowledge_grob (Grob_info inf)
{
  if (Stem::has_interface (inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size (); i++)
	{
	  Grob*e = script_p_arr_[i];

	  e->set_grob_property ("direction-source", inf.elem_l_->self_scm ());
	  e->add_dependency (inf.elem_l_);
	  Side_position_interface::add_support (e, inf.elem_l_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.elem_l_))
    {
      for (int i=0; i < script_p_arr_.size (); i++)
	{
	  Grob *e = script_p_arr_[i];
	  
	  if (!e->parent_l (X_AXIS))
	    {
	      e->set_parent (inf.elem_l_, X_AXIS);
	    }
	  if (Side_position_interface::get_axis (e) == X_AXIS
	      && !e->parent_l (Y_AXIS))
	    {
	      e->set_parent (inf.elem_l_, Y_AXIS);
	      e->add_dependency (inf.elem_l_); // ??
	    }
	  Side_position_interface::add_support (e,inf.elem_l_);
	}
    }  
}

void
Script_engraver::stop_translation_timestep ()
{
  for (int i=0; i < script_p_arr_.size (); i++) 
    {
      Grob * sc = script_p_arr_[i];
      if (to_boolean (sc->get_grob_property ("staff-support")))
	{
	  Side_position_interface::add_staff_support (sc);
	}
      typeset_grob (sc);
    }
  script_p_arr_.clear ();
}

void
Script_engraver::start_translation_timestep ()
{
  script_req_l_arr_.clear ();
}

ADD_THIS_TRANSLATOR (Script_engraver);


