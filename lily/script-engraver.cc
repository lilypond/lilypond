/*
  script-engraver.cc -- implement Script_engraver

  (c)  1997--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "script.hh"
#include "side-position-interface.hh"
#include "event.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver.hh"
#include "note-column.hh"
#include "translator-group.hh"

struct Script_tuple
{
  Music *event_;
  Grob * script_;
  SCM description_;
  Script_tuple ()
  {
    event_ = 0;
    script_ = 0;
    description_ = SCM_EOL;
  }
};

class Script_engraver : public Engraver
{
  Array<Script_tuple> scripts_;
public:
  TRANSLATOR_DECLARATIONS(Script_engraver);
protected:
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);
};

bool
Script_engraver::try_music (Music *r)
{
  if (r->is_mus_type ("articulation-event"))
    {
      Script_tuple t;
      t.event_ =r;
      scripts_.push (t);
      return true;
    }
  return false;
}

void
copy_property (Grob * g , SCM sym, SCM alist)
{
  if (g->internal_get_grob_property (sym) == SCM_EOL)
    {
      SCM entry = scm_assoc (sym,alist);
      if (gh_pair_p (entry))
	{
	  g->internal_set_grob_property (sym, gh_cdr (entry));
	}
    }
}


/*
  We add the properties, one by one for each Script. We could save a
  little space by tacking the props onto the Script grob (i.e. make
  ScriptStaccato , ScriptMarcato, etc. )
 */
Grob *make_script_from_event (SCM * descr, Translator_group*tg, Music * event,
			      int index)
{
  SCM alist = tg->get_property ("scriptDefinitions");
  SCM art_type= event->get_mus_property ("articulation-type");
  SCM art = scm_assoc (art_type, alist);

  if (art == SCM_BOOL_F)
    {
      event->origin ()->warning (_("Don't know how to interpret articulation:"));
      event->origin ()->warning (_("Scheme encoding: "));
      scm_write (art_type, scm_current_error_port ());
      return 0 ;
    }

  art = gh_cdr (art);
    
  Grob *p =new Item (tg->get_property ("Script"));
  *descr = art;  

  SCM force_dir = event->get_mus_property ("direction");
  if (is_direction (force_dir) && to_dir (force_dir))
    p->set_grob_property ("direction", force_dir);

  copy_property (p, ly_symbol2scm ("script-molecule"), art);
  copy_property (p, ly_symbol2scm ("direction"), art);
  copy_property (p, ly_symbol2scm ("side-relative-direction"), art);

  int prio =0;
  SCM sprio = scm_assoc (ly_symbol2scm ("script-priority"), art);
  if (gh_pair_p (sprio))
    prio = gh_scm2int (gh_cdr (sprio));


  /*
    Make sure they're in order of user input by adding index i.
    Don't use the direction in this priority. Smaller means closer
    to the head.
  */
  prio += index;

  Side_position_interface::set_axis (p, Y_AXIS);
  p->set_grob_property ("script-priority", gh_int2scm (prio));
  return p;
}

void
Script_engraver::process_music ()
{
  for (int i=0; i < scripts_.size (); i++)
    {
      Music* l=scripts_[i].event_;

      Grob * p = make_script_from_event (&scripts_[i].description_, daddy_trans_, l, i);

      scripts_[i].script_ = p;
      if (p)
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
	  Grob*e = scripts_[i].script_;

	  if (to_dir (e->get_grob_property ("side-relative-direction")))
	    e->set_grob_property ("direction-source", inf.grob_->self_scm ());

	  /*
	    add dep ? 
	   */
	  e->add_dependency (inf.grob_);
	  Side_position_interface::add_support (e, inf.grob_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.grob_))
    {
      for (int i=0; i < scripts_.size (); i++)
	{
	  Grob *e = scripts_[i].script_;
	  
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
	  Grob *e = scripts_[i].script_;
	  
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
      if (!scripts_[i].script_)
	continue;
      
      Grob * sc = scripts_[i].script_;

      SCM follow = scm_assoc (ly_symbol2scm ("follow-into-staff"), scripts_[i].description_);
      if (gh_pair_p (follow) && to_boolean (gh_cdr (follow)))
	sc->add_offset_callback (Side_position_interface::quantised_position_proc, Y_AXIS);
      else
	Side_position_interface::add_staff_support (sc);
      typeset_grob (sc);
    }
  scripts_.clear ();
}

void
Script_engraver::start_translation_timestep ()
{
  scripts_.clear ();
}



Script_engraver::Script_engraver(){}

ENTER_DESCRIPTION(Script_engraver,
/* descr */       "Handles note scripted articulations.",
/* creats*/       "Script",
/* accepts */     "script-event articulation-event",
/* acks  */      "stem-interface rhythmic-head-interface note-column-interface",
/* reads */       "scriptDefinitions",
/* write */       "");
