/*
  script-engraver.cc -- engrave Scripts: Articulations.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "context.hh"
#include "directional-element-interface.hh"
#include "engraver.hh"
#include "event.hh"
#include "slur.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "warn.hh"

struct Script_tuple
{
  Music *event_;
  Grob *script_;
  bool follow_into_staff_;
  Script_tuple ()
  {
    follow_into_staff_ = false;
    event_ = 0;
    script_ = 0;
  }
};

class Script_engraver : public Engraver
{
  Array<Script_tuple> scripts_;
  Spanner *slur_;

protected:
  virtual bool try_music (Music*);
  virtual void stop_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);

public:
  TRANSLATOR_DECLARATIONS (Script_engraver);
};

Script_engraver::Script_engraver ()
{
  slur_ = 0;
}

bool
Script_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("articulation-event"))
    {
      /* Discard double articulations for part-combining.  */
      int script_count = scripts_.size ();
      for (int i = 0; i < script_count; i++)
	if (ly_c_equal_p (scripts_[i].event_
			  ->get_property ("articulation-type"),
			  m->get_property ("articulation-type")))
	  return true;

      Script_tuple t;
      t.event_ = m;
      scripts_.push (t);
      return true;
    }
  return false;
}

void
copy_property (Grob *g, SCM sym, SCM alist)
{
  if (g->internal_get_property (sym) == SCM_EOL)
    {
      SCM entry = scm_assoc (sym, alist);
      if (scm_is_pair (entry))
	g->internal_set_property (sym, scm_cdr (entry));
    }
}

/* Add the properties, one by one for each Script.  A little memory
   could be saved by tacking the props onto the Script grob (i.e. make
   ScriptStaccato , ScriptMarcato, etc. ).

*/
void make_script_from_event (Grob *p, bool * follow, Context *tg,
			     SCM art_type, int index)
{
  SCM alist = tg->get_property ("scriptDefinitions");
  SCM art = scm_assoc (art_type, alist);

  if (art == SCM_BOOL_F)
    {
      /* FIXME: */
      warning (_ ("Do not know how to interpret articulation: "));
      warning (_ ("Scheme encoding: "));
      scm_write (art_type, scm_current_error_port ());
      return;
    }

  art = scm_cdr (art);

  SCM follow_scm = scm_assoc (ly_symbol2scm ("follow-into-staff"),
			      art);

  *follow = scm_is_pair (follow_scm) && to_boolean (scm_cdr (follow_scm));
  bool priority_found = false ; 
    
  for (SCM s = art ; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
      if (!ly_c_procedure_p (type))
	continue;
      
      SCM val = scm_cdar (s);

      if (sym == ly_symbol2scm ("script-priority"))
	{
	  priority_found = true;
	  /* Make sure they're in order of user input by adding index i.
	     Don't use the direction in this priority. Smaller means closer
	     to the head.  */
	  int prio  = scm_to_int (val) +  index;
	  
	 
	  val = scm_int2num (prio);
	}
      if (p->internal_get_property (sym) == SCM_EOL)
	p->internal_set_property (sym, val);
    }

  if (!priority_found)
    {
      p->set_property ("script-priority",
		       scm_int2num (index));
    }
  
  Side_position_interface::set_axis (p, Y_AXIS);

}

void
Script_engraver::process_music ()
{
  int script_count = scripts_.size ();
  for (int i = 0; i < script_count; i++)
    {
      Music *m = scripts_[i].event_;

      Grob *p = make_item ("Script", m->self_scm ());

      make_script_from_event (p, &scripts_[i].follow_into_staff_, context (),
			      m->get_property ("articulation-type"),
			      i);

      scripts_[i].script_ = p;

      SCM force_dir = m->get_property ("direction");
      if (is_direction (force_dir) && to_dir (force_dir))
	p->set_property ("direction", force_dir);
    }
}

void
Script_engraver::acknowledge_grob (Grob_info info)
{
  int script_count = scripts_.size ();
  if (Stem::has_interface (info.grob_))
    {
      for (int i = 0; i < script_count; i++)
	{
	  Grob *e = scripts_[i].script_;

	  if (to_dir (e->get_property ("side-relative-direction")))
	    e->set_property ("direction-source", info.grob_->self_scm ());

	  /* FIXME: add dependency */
	  e->add_dependency (info.grob_);
	  Side_position_interface::add_support (e, info.grob_);
	}
    }
  else if (Rhythmic_head::has_interface (info.grob_)
	   && info.music_cause ())
    {
      for (int i = 0; i < script_count; i++)
	{
	  Grob *e = scripts_[i].script_;
	
	  if (Side_position_interface::get_axis (e) == X_AXIS
	      && !e->get_parent (Y_AXIS))
	    {
	      e->set_parent (info.grob_, Y_AXIS);
	      e->add_dependency (info.grob_);
	    }
	  Side_position_interface::add_support (e,info.grob_);
	}
    }
  else if (Note_column::has_interface (info.grob_))
    {
      /* Make note column the parent of the script.  That is not
	correct, but due to seconds in a chord, noteheads may be
	swapped around horizontally.

	As the note head to put it on is not known now, postpone this
	decision to Script_interface::before_line_breaking ().  */
      for (int i = 0; i < script_count; i++)
	{
	  Grob *e = scripts_[i].script_;

	  if (!e->get_parent (X_AXIS)
	      && Side_position_interface::get_axis (e) == Y_AXIS)
	    e->set_parent (info.grob_, X_AXIS);
	}
    }
  else if (Slur::has_interface (info.grob_))
    slur_ = dynamic_cast<Spanner*> (info.grob_);
}

void
Script_engraver::stop_translation_timestep ()
{
  int script_count = scripts_.size ();
  for (int i = 0; i < script_count; i++)
    if (scripts_[i].follow_into_staff_)
      {
	Grob *sc = scripts_[i].script_;
	sc->add_offset_callback (Side_position_interface
				 ::quantised_position_proc, Y_AXIS);
	sc->set_property ("staff-padding", SCM_EOL);
      }
  
  scripts_.clear ();
}

ENTER_DESCRIPTION (Script_engraver,
/* descr */       "Handles note scripted articulations.",
/* creats*/       "Script",
/* accepts */     "script-event articulation-event",
/* acks  */       "stem-interface rhythmic-head-interface " 
		  "slur-interface note-column-interface",
/* reads */       "scriptDefinitions",
/* write */       "");
