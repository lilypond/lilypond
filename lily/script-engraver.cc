/*
  script-engraver.cc -- engrave Scripts: Articulations.

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "context.hh"
#include "directional-element-interface.hh"
#include "international.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "side-position-interface.hh"
#include "slur.hh"
#include "staff-symbol-referencer.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

struct Script_tuple
{
  Stream_event *event_;
  Grob *script_;
  Script_tuple ()
  {
    event_ = 0;
    script_ = 0;
  }
};

class Script_engraver : public Engraver
{
  vector<Script_tuple> scripts_;

protected:
  void stop_translation_timestep ();
  void process_music ();

  DECLARE_TRANSLATOR_LISTENER (articulation);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  DECLARE_ACKNOWLEDGER (stem);
  DECLARE_ACKNOWLEDGER (stem_tremolo);
  DECLARE_ACKNOWLEDGER (note_column);

public:
  TRANSLATOR_DECLARATIONS (Script_engraver);
};

Script_engraver::Script_engraver ()
{
}

IMPLEMENT_TRANSLATOR_LISTENER (Script_engraver, articulation);
void
Script_engraver::listen_articulation (Stream_event *ev)
{
  /* Discard double articulations for part-combining.  */
  int script_count = scripts_.size ();
  for (int i = 0; i < script_count; i++)
    if (ly_is_equal (scripts_[i].event_
		     ->get_property ("articulation-type"),
		     ev->get_property ("articulation-type")))
      return;

  Script_tuple t;
  t.event_ = ev;
  scripts_.push_back (t);
}

void
copy_property (Grob *g, SCM sym, SCM alist)
{
  if (g->internal_get_property (sym) == SCM_EOL)
    {
      SCM entry = scm_assoc (sym, alist);
      if (scm_is_pair (entry))
	g->set_property (sym, scm_cdr (entry));
    }
}

/* Add the properties, one by one for each Script.  A little memory
   could be saved by tacking the props onto the Script grob (i.e. make
   ScriptStaccato , ScriptMarcato, etc. ).
*/
void
make_script_from_event (Grob *p,  Context *tg,
			SCM art_type, int index)
{
  SCM alist = tg->get_property ("scriptDefinitions");
  SCM art = scm_assoc (art_type, alist);

  if (art == SCM_BOOL_F)
    {
      /* FIXME: */
      warning (_ ("do not know how to interpret articulation: "));
      warning (_ ("scheme encoding: "));
      scm_write (art_type, scm_current_error_port ());
      message ("");
      return;
    }

  art = scm_cdr (art);

  bool priority_found = false;

  for (SCM s = art; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM sym = scm_caar (s);
      SCM type = scm_object_property (sym, ly_symbol2scm ("backend-type?"));
      if (!ly_is_procedure (type))
	continue;

      SCM val = scm_cdar (s);

      if (sym == ly_symbol2scm ("script-priority"))
	{
	  priority_found = true;
	  /* Make sure they're in order of user input by adding index i.
	     Don't use the direction in this priority. Smaller means closer
	     to the head.  */
	  int prio = scm_to_int (val) + index;

	  val = scm_from_int (prio);
	}

      SCM preset = p->get_property_data (sym);
      if (val == SCM_EOL
	  || scm_call_1 (type, preset) == SCM_BOOL_F)
	p->set_property (sym, val);
    }

  if (!priority_found)
    {
      p->set_property ("script-priority",
		       scm_from_int (index));
    }
}

void
Script_engraver::process_music ()
{
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Stream_event *ev = scripts_[i].event_;

      Grob *p = make_item ("Script", ev->self_scm ());

      make_script_from_event (p, context (),
			      ev->get_property ("articulation-type"),
			      i);

      scripts_[i].script_ = p;

      SCM force_dir = ev->get_property ("direction");
      if (is_direction (force_dir) && to_dir (force_dir))
	p->set_property ("direction", force_dir);
    }
}

void
Script_engraver::acknowledge_stem (Grob_info info)
{
  int script_count = scripts_.size ();
  for (int i = 0; i < script_count; i++)
    {
      Grob *e = scripts_[i].script_;

      if (to_dir (e->get_property ("side-relative-direction")))
	e->set_object ("direction-source", info.grob ()->self_scm ());

      Side_position_interface::add_support (e, info.grob ());
    }
}

void
Script_engraver::acknowledge_stem_tremolo (Grob_info info)
{
  int script_count = scripts_.size ();
  for (int i = 0; i < script_count; i++)
    {
      Grob *e = scripts_[i].script_;
      Side_position_interface::add_support (e, info.grob ());
    }
}


void
Script_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  if (info.event_cause ())
    {
      for (vsize i = 0; i < scripts_.size (); i++)
 	{
	  Grob *e = scripts_[i].script_;

	  if (Side_position_interface::get_axis (e) == X_AXIS
	      && !e->get_parent (Y_AXIS))
	    {
	      e->set_parent (info.grob (), Y_AXIS);
	    }
	  Side_position_interface::add_support (e, info.grob ());
	}
    }
}

void
Script_engraver::acknowledge_note_column (Grob_info info)
{
  /* Make note column the parent of the script.  That is not
     correct, but due to seconds in a chord, noteheads may be
     swapped around horizontally.

     As the note head to put it on is not known now, postpone this
     decision to Script_interface::calc_direction ().  */
  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *e = scripts_[i].script_;

      if (!e->get_parent (X_AXIS)
	  && Side_position_interface::get_axis (e) == Y_AXIS)
	e->set_parent (info.grob (), X_AXIS);
    }
}

void
Script_engraver::stop_translation_timestep ()
{
  scripts_.clear ();
}

ADD_ACKNOWLEDGER (Script_engraver, rhythmic_head);
ADD_ACKNOWLEDGER (Script_engraver, stem);
ADD_ACKNOWLEDGER (Script_engraver, note_column);
ADD_ACKNOWLEDGER (Script_engraver, stem_tremolo);

ADD_TRANSLATOR (Script_engraver,
		/* doc */
		"Handle note scripted articulations.",

		/* create */
		"Script ",

		/* read */
		"scriptDefinitions ",

		/* write */
		""
		);
