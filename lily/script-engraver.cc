/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script.hh"
#include "side-position-interface.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"
#include "note-column.hh"

class Script_engraver : public Engraver
{
  Link_array<Grob> scripts_;
  Link_array<Articulation_req> script_reqs_;

public:
  TRANSLATOR_DECLARATIONS(Script_engraver);
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
  script_reqs_.clear ();
}

bool
Script_engraver::try_music (Music *r)
{
  if (Articulation_req *mr = dynamic_cast <Articulation_req *> (r))
    {
      for (int i=0; i < script_reqs_.size (); i++) 
	{
	  if (script_reqs_[i]->equal_b (mr))
	    return true;
	}
      script_reqs_.push (mr);
      return true;
    }
  return false;
}

void
Script_engraver::process_music ()
{
  for (int i=0; i < script_reqs_.size (); i++)
    {
      Articulation_req* l=script_reqs_[i];

      SCM alist = get_property ("scriptDefinitions");
      SCM art = scm_assoc (l->get_mus_property ("articulation-type"), alist);

      if (art == SCM_BOOL_F)
	{
	  String a = ly_scm2string (l->get_mus_property ("articulation-type"));
	  l->origin ()->warning (_f ("Don't know how to interpret articulation `%s'", a.to_str0 ()));
			
	  continue;
	}
      // todo -> use result of articulation-to-scriptdef directly as basic prop list.
      Grob *p =new Item (get_property ("Script"));
      art = ly_cdr (art);
      p->set_grob_property ("script-molecule", ly_car (art));

      art = ly_cdr (art);
      bool follow_staff = gh_scm2bool (ly_car (art));
      art = ly_cdr (art);
      SCM relative_stem_dir = ly_car (art);
      art = ly_cdr (art);

      SCM force_dir = l->get_mus_property ("direction");
      if (ly_dir_p (force_dir) && !to_dir (force_dir))
	force_dir = ly_car (art);
      
      art = ly_cdr (art);
      int priority = gh_scm2int (ly_car (art));

      SCM s = p->get_grob_property ("script-priority");
      if (gh_number_p (s))
	priority = gh_scm2int (s);

      /* Make sure they're in order of user input by adding index i.
      Don't use the direction in this priority. Smaller means closer
      to the head.
      */
      priority += i;

      if (ly_dir_p (force_dir) && to_dir (force_dir))
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
      
      
      p->set_grob_property ("script-priority", gh_int2scm (priority));
  
      scripts_.push (p);
      
      announce_grob (p, l->self_scm());
    }
}

void
Script_engraver::acknowledge_grob (Grob_info inf)
{
  if (Stem::has_interface (inf.grob_))
    {
      for (int i=0; i < scripts_.size (); i++)
	{
	  Grob*e = scripts_[i];

	  e->set_grob_property ("direction-source", inf.grob_->self_scm ());
	  e->add_dependency (inf.grob_);
	  Side_position_interface::add_support (e, inf.grob_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.grob_))
    {
      for (int i=0; i < scripts_.size (); i++)
	{
	  Grob *e = scripts_[i];
	  
	  if (Side_position_interface::get_axis (e) == X_AXIS
	      && !e->get_parent (Y_AXIS))
	    {
	      e->set_parent (inf.grob_, Y_AXIS);
	      e->add_dependency (inf.grob_); // ??
	    }
	  Side_position_interface::add_support (e,inf.grob_);
	}
    }
  else if (Note_column::has_interface (inf.grob_))
    {

      /*
	We make note column the parent of the script. That's not
	correct, but due to seconds in a chord, noteheads may be
	swapped around horizontally. We don't know which note head to
	put it on, so we postpone this decision to
	Script_interface::before_line_breaking ().
 
       */
      for (int i=0; i < scripts_.size (); i++)
	{
	  Grob *e = scripts_[i];
	  
	  if (!e->get_parent (X_AXIS) &&
	      Side_position_interface::get_axis (e) == Y_AXIS)
	    {
	      e->set_parent (inf.grob_, X_AXIS);
	    }
	}
    }
}

void
Script_engraver::stop_translation_timestep ()
{
  for (int i=0; i < scripts_.size (); i++) 
    {

      /*
	TODO: junk staff-support.
       */
      Grob * sc = scripts_[i];
      if (to_boolean (sc->get_grob_property ("staff-support")))
	{
	  Side_position_interface::add_staff_support (sc);
	}
      typeset_grob (sc);
    }
  scripts_.clear ();
}

void
Script_engraver::start_translation_timestep ()
{
  script_reqs_.clear ();
}



Script_engraver::Script_engraver(){}

ENTER_DESCRIPTION(Script_engraver,
/* descr */       "    Handles note ornaments generated by @code{\\script}.  
",
/* creats*/       "Script",
/* acks  */       "stem-interface rhythmic-head-interface note-column-interface",
/* reads */       "scriptDefinitions scriptHorizontal",
/* write */       "");
