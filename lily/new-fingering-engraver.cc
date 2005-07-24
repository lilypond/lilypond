/*
  fingering-engraver.cc -- implement New_fingering_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "warn.hh"
#include "engraver.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"
#include "script-interface.hh"
#include "stem.hh"

#include "translator.icc"

struct Finger_tuple
{
  Grob *head_;
  Grob *script_;
  Music *note_event_;
  Music *finger_event_;
  bool follow_into_staff_;
  int position_;

  Finger_tuple ()
  {
    position_ = 0;
    head_ = script_ = 0;
    note_event_ = finger_event_ = 0;
    follow_into_staff_ = false;
  }
  static int compare (Finger_tuple const &c1, Finger_tuple const &c2)
  {
    return c1.position_- c2.position_;
  }
};

class New_fingering_engraver : public Engraver
{
  Array<Finger_tuple> fingerings_;
  Array<Finger_tuple> articulations_;
  Array<Finger_tuple> string_numbers_;
  
  Link_array<Grob> heads_;
  Grob *stem_;

public:
  TRANSLATOR_DECLARATIONS (New_fingering_engraver);
protected:
  PRECOMPUTED_VIRTUAL void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER(rhythmic_head);
  DECLARE_ACKNOWLEDGER(stem);
  void add_fingering (Grob *, Music *, Music *);
  void add_script (Grob *, Music *, Music *);
  void add_string (Grob *, Music *, Music *);
  void position_scripts (SCM orientations, Array<Finger_tuple> *);
};

void
New_fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  Music *note_ev = inf.music_cause ();
  if (!note_ev)
    return;

  SCM arts = note_ev->get_property ("articulations");

  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Music *m = unsmob_music (scm_car (s));

      if (!m)
	continue;

      if (m->is_mus_type ("fingering-event"))
	{
	  add_fingering (inf.grob (), m, note_ev);
	}
      else if (m->is_mus_type ("text-script-event"))
	{
	  m->origin ()->warning (_ ("can't add text scripts to individual note heads"));
	}
      else if (m->is_mus_type ("script-event"))
	{
	  add_script (inf.grob (), m, note_ev);
	}
      else if (m->is_mus_type ("string-number-event"))
	{
	  add_string (inf.grob (), m, note_ev);
	}
      else if (m->is_mus_type ("harmonic-event"))
	{
	  inf.grob ()->set_property ("style", ly_symbol2scm ("harmonic"));
	  Grob *d = unsmob_grob (inf.grob ()->get_object ("dot"));
	  if (d)
	    d->suicide ();
	}
    }

  heads_.push (inf.grob ());
}

void
New_fingering_engraver::acknowledge_stem (Grob_info inf)
{
  stem_ = inf.grob ();
}

void
New_fingering_engraver::add_script (Grob *head,
				    Music *event,
				    Music *note)
{
  (void) note;

  Finger_tuple ft;

  Grob *g = make_item ("Script", event->self_scm ());
  make_script_from_event (g, &ft.follow_into_staff_, context (),
			  event->get_property ("articulation-type"), 0);
  if (g)
    {
      ft.script_ = g;

      articulations_.push (ft);

      ft.script_->set_parent (head, X_AXIS);
    }
}

void
New_fingering_engraver::add_fingering (Grob *head,
				       Music *event,
				       Music *hevent)
{
  Finger_tuple ft;

  ft.script_ = make_item ("Fingering", event->self_scm ());

  Side_position_interface::add_support (ft.script_, head);

  int d = scm_to_int (event->get_property ("digit"));

  /*
    TODO:

    Should add support for thumb.  It's a little involved, since
    the thumb lives in a different font. Maybe it should be moved?

  */
  if (d > 5)
    {
      /*
	music for the softenon children?
      */
      event->origin ()->warning (_ ("music for the martians."));
    }
  SCM sstr = scm_number_to_string (scm_int2num (d), scm_int2num (10));
  ft.script_->set_property ("text", sstr);

  ft.finger_event_ = event;
  ft.note_event_ = hevent;
  ft.head_ = head;

  fingerings_.push (ft);
}


void
New_fingering_engraver::add_string (Grob *head,
				    Music *event,
				    Music *hevent)
{
  Finger_tuple ft;

  ft.script_ = make_item ("StringNumber", event->self_scm ());

  Side_position_interface::add_support (ft.script_, head);

  int d = scm_to_int (event->get_property ("string-number"));

  SCM sstr = scm_number_to_string (scm_int2num (d), scm_int2num (10));
  ft.script_->set_property ("text", sstr);

  ft.finger_event_ = event;
  ft.note_event_ = hevent;
  ft.head_ = head;

  string_numbers_.push (ft);
}

void
New_fingering_engraver::position_scripts (SCM orientations,
					  Array<Finger_tuple> *scripts)
{
  for (int i = 0; i < scripts->size (); i++)
    if (stem_ && to_boolean (scripts->elem (i).script_->get_property ("add-stem-support")))
      Side_position_interface::add_support (scripts->elem (i).script_, stem_);

  /*
    This is not extremely elegant, but we have to do a little
    formatting here, because the parent/child relations should be
    known before we move on to the next time step.

    A more sophisticated approach would be to set both X and Y parents
    to the note head, and write a more flexible function for
    positioning the fingerings, setting both X and Y coordinates.
  */
  for (int i = 0; i < scripts->size (); i++)
    {
      (*scripts)[i].position_ = scm_to_int ((*scripts)[i].head_->get_property ("staff-position"));
    }

  for (int i = scripts->size (); i--;)
    for (int j = heads_.size (); j--;)
      Side_position_interface::add_support ((*scripts)[i].script_, heads_[j]);

  Array<Finger_tuple> up, down, horiz;
  for (int i = scripts->size (); i--;)
    {
      SCM d = (*scripts)[i].finger_event_->get_property ("direction");
      if (to_dir (d))
	{
	  ((to_dir (d) == UP) ? up : down).push ((*scripts)[i]);
	  scripts->del (i);
	}
    }

  scripts->sort (&Finger_tuple::compare);

  bool up_p = scm_c_memq (ly_symbol2scm ("up"), orientations) != SCM_BOOL_F;
  bool down_p = scm_c_memq (ly_symbol2scm ("down"), orientations) != SCM_BOOL_F;
  bool left_p = scm_c_memq (ly_symbol2scm ("left"), orientations) != SCM_BOOL_F;
  bool right_p = scm_c_memq (ly_symbol2scm ("right"), orientations) != SCM_BOOL_F;
  Direction hordir = (right_p) ? RIGHT : LEFT;
  if (left_p || right_p)
    {
      if (up_p && !up.size () && scripts->size ())
	up.push (scripts->pop ());

      if (down_p && !down.size () && scripts->size ())
	{
	  down.push ((*scripts)[0]);
	  scripts->del (0);
	}

      horiz.concat (*scripts);
    }
  else if (up_p && down_p)
    {
      int center = scripts->size () / 2;
      down.concat (scripts->slice (0, center));
      up.concat (scripts->slice (center, scripts->size ()));
    }
  else if (up_p)
    {
      up.concat (*scripts);
      scripts->clear ();
    }
  else
    {
      if (!down_p)
	{
	  warning (_ ("no placement found for fingerings"));
	  warning (_ ("placing below"));
	}
      down.concat (*scripts);
      scripts->clear ();
    }

  for (int i = 0; i < horiz.size (); i++)
    {
      Finger_tuple ft = horiz[i];
      Grob *f = ft.script_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_parent (ft.head_, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, Y_AXIS);
      f->add_offset_callback (Side_position_interface::aligned_side_proc, X_AXIS);

      f->set_property ("direction", scm_int2num (hordir));
    }

  int finger_prio = 200;
  for (int i = 0; i < up.size (); i++)
    {
      Finger_tuple ft = up[i];
      Grob *f = ft.script_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_property ("script-priority",
		       scm_int2num (finger_prio + ft.position_));
      f->add_offset_callback (Side_position_interface::aligned_side_proc, Y_AXIS);
      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, X_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, X_AXIS);

      f->set_property ("direction", scm_int2num (UP));
    }

  for (int i = 0; i < down.size (); i++)
    {
      Finger_tuple ft = down[i];
      Grob *f = ft.script_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_property ("script-priority",
		       scm_int2num (finger_prio + down.size () - ft.position_));

      f->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, X_AXIS);
      f->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, X_AXIS);
      f->add_offset_callback (Side_position_interface::aligned_side_proc, Y_AXIS);
      f->set_property ("direction", scm_int2num (DOWN));
    }
}

void
New_fingering_engraver::stop_translation_timestep ()
{
  if (fingerings_.size ())
    {
      position_scripts (get_property ("fingeringOrientations"),
			&fingerings_);
      fingerings_.clear ();
    }

  if (string_numbers_.size ())
    {
      position_scripts (get_property ("stringNumberOrientations"),
			&string_numbers_);
      string_numbers_.clear ();
    }

  for (int i = articulations_.size (); i--;)
    {
      Grob *script = articulations_[i].script_;

      for (int j = heads_.size (); j--;)
	Side_position_interface::add_support (script, heads_[j]);

      if (stem_ && to_dir (script->get_property ("side-relative-direction")))
	script->set_object ("direction-source", stem_->self_scm ());

      if (stem_ && to_boolean (script->get_property ("add-stem-support")))
	Side_position_interface::add_support (script, stem_);

      if (articulations_[i].follow_into_staff_)
	{
	  script->add_offset_callback (Side_position_interface::quantised_position_proc, Y_AXIS);
	  script->set_property ("staff-padding", SCM_EOL);
	}
    }

  stem_ = 0;
  heads_.clear ();
  articulations_.clear ();
}

New_fingering_engraver::New_fingering_engraver ()
{
  stem_ = 0;
}
ADD_ACKNOWLEDGER(New_fingering_engraver, rhythmic_head);
ADD_ACKNOWLEDGER(New_fingering_engraver, stem);

ADD_TRANSLATOR (New_fingering_engraver,
		/* descr */ "Create fingering-scripts for notes in a new chord.  "
		"This engraver is ill-named, since it "
		"also takes care of articulations and harmonic note heads",
		/* creats*/ "Fingering",
		/* accepts */ "",
		/* reads */ "fingeringOrientations",
		/* write */ "");
