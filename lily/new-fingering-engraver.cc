/*
  fingering-engraver.cc -- implement New_fingering_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"

#include "international.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "item.hh"
#include "warn.hh"

#include "translator.icc"



struct Finger_tuple
{
  Grob *head_;
  Grob *script_;
  Stream_event *note_event_;
  Stream_event *finger_event_;
  bool follow_into_staff_;
  int position_;

  Finger_tuple ()
  {
    position_ = 0;
    head_ = script_ = 0;
    note_event_ = finger_event_ = 0;
    follow_into_staff_ = false;
  }
};

bool
operator< (Finger_tuple const &a, Finger_tuple const &b)
{
  return a.position_ < b.position_;
}

class New_fingering_engraver : public Engraver
{
  vector<Finger_tuple> fingerings_;
  vector<Finger_tuple> stroke_fingerings_;
  vector<Finger_tuple> articulations_;
  vector<Finger_tuple> string_numbers_;

  vector<Grob*> heads_;
  Grob *stem_;

  void position_all ();
public:
  TRANSLATOR_DECLARATIONS (New_fingering_engraver);
protected:
  void stop_translation_timestep ();
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  DECLARE_ACKNOWLEDGER (stem);
  void add_fingering (Grob *, SCM,
		      vector<Finger_tuple> *,
		      Stream_event *, Stream_event *);
  void add_script (Grob *, Stream_event *, Stream_event *);
  void add_string (Grob *, Stream_event *, Stream_event *);
  void position_scripts (SCM orientations, vector<Finger_tuple> *);
};

void
New_fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  Stream_event *note_ev = inf.event_cause ();
  if (!note_ev)
    return;

  SCM arts = note_ev->get_property ("articulations");

  for (SCM s = arts; scm_is_pair (s); s = scm_cdr (s))
    {
      Stream_event *ev = unsmob_stream_event (scm_car (s));

      if (!ev)
	continue;

      if (ev->in_event_class ("fingering-event"))
	add_fingering (inf.grob (),
		       ly_symbol2scm ("Fingering"),
		       &fingerings_,
		       ev, note_ev);
      else if (ev->in_event_class ("text-script-event"))
	ev->origin ()->warning (_ ("cannot add text scripts to individual note heads"));
      else if (ev->in_event_class ("script-event"))
	add_script (inf.grob (), ev, note_ev);
      else if (ev->in_event_class ("string-number-event"))
	add_fingering (inf.grob (),
		       ly_symbol2scm ("StringNumber"), &string_numbers_,
		       ev, note_ev);
      else if (ev->in_event_class ("stroke-finger-event"))
	add_fingering (inf.grob (),
		       ly_symbol2scm ("StrokeFinger"), &stroke_fingerings_,
		       ev, note_ev);
      else if (ev->in_event_class ("harmonic-event"))
	{
	  inf.grob ()->set_property ("style", ly_symbol2scm ("harmonic"));
	  Grob *d = unsmob_grob (inf.grob ()->get_object ("dot"));
	  if (d && !to_boolean (get_property ("harmonicDots")))
	    d->suicide ();
	}
    }

  heads_.push_back (inf.grob ());
}

void
New_fingering_engraver::acknowledge_stem (Grob_info inf)
{
  stem_ = inf.grob ();
}

void
New_fingering_engraver::add_script (Grob *head,
				    Stream_event *event,
				    Stream_event *note)
{
  (void) note;

  Finger_tuple ft;

  Grob *g = make_item ("Script", event->self_scm ());
  make_script_from_event (g, context (),
			  event->get_property ("articulation-type"), 0);
  ft.script_ = g;
  ft.script_->set_parent (head, X_AXIS);
  
  articulations_.push_back (ft);
}

void
New_fingering_engraver::add_fingering (Grob *head,
				       SCM grob_sym,
				       vector<Finger_tuple> *tuple_vector,
				       Stream_event *event,
				       Stream_event *hevent)
{
  Finger_tuple ft;

  ft.script_ = internal_make_item (grob_sym, event->self_scm (),
				   ly_symbol2string (grob_sym).c_str (),
				   __FILE__, __LINE__, __FUNCTION__
				   );

  Side_position_interface::add_support (ft.script_, head);

  ft.finger_event_ = event;
  ft.note_event_ = hevent;
  ft.head_ = head;

  tuple_vector->push_back (ft);
}

void
New_fingering_engraver::position_scripts (SCM orientations,
					  vector<Finger_tuple> *scripts)
{
  for (vsize i = 0; i < scripts->size (); i++)
    if (stem_ && to_boolean (scripts->at (i).script_->get_property ("add-stem-support")))
      Side_position_interface::add_support (scripts->at (i).script_, stem_);

  /*
    This is not extremely elegant, but we have to do a little
    formatting here, because the parent/child relations should be
    known before we move on to the next time step.

    A more sophisticated approach would be to set both X and Y parents
    to the note head, and write a more flexible function for
    positioning the fingerings, setting both X and Y coordinates.
  */
  for (vsize i = 0; i < scripts->size (); i++)
    (*scripts)[i].position_ = scm_to_int ((*scripts)[i].head_->get_property ("staff-position"));

  for (vsize i = scripts->size (); i--;)
    for (vsize j = heads_.size (); j--;)
      Side_position_interface::add_support ((*scripts)[i].script_, heads_[j]);

  vector<Finger_tuple> up, down, horiz;
  for (vsize i = scripts->size (); i--;)
    {
      SCM d = (*scripts)[i].finger_event_->get_property ("direction");
      if (to_dir (d))
	{
	  ((to_dir (d) == UP) ? up : down).push_back ((*scripts)[i]);
	  scripts->erase (scripts->begin () + i);
	}
    }

  vector_sort (*scripts, less<Finger_tuple> ());

  bool up_p = scm_c_memq (ly_symbol2scm ("up"), orientations) != SCM_BOOL_F;
  bool down_p = scm_c_memq (ly_symbol2scm ("down"), orientations) != SCM_BOOL_F;
  bool left_p = scm_c_memq (ly_symbol2scm ("left"), orientations) != SCM_BOOL_F;
  bool right_p = scm_c_memq (ly_symbol2scm ("right"), orientations) != SCM_BOOL_F;
  Direction hordir = (right_p) ? RIGHT : LEFT;
  if (left_p || right_p)
    {
      if (up_p && !up.size () && scripts->size ())
	{
	  up.push_back (scripts->back ());
	  scripts->pop_back ();
	}

      if (down_p && !down.size () && scripts->size ())
	{
	  down.push_back ((*scripts)[0]);
	  scripts->erase (scripts->begin ());
	}

      horiz.insert (horiz.end (), scripts->begin (), scripts->end ());
    }
  else if (up_p && down_p)
    {
      int center = scripts->size () / 2;
      down.insert (down.end (), scripts->begin (), scripts->begin () + center);
      up.insert (up.end (), scripts->begin () + center, scripts->end ());
    }
  else if (up_p)
    {
      up.insert (up.end (), scripts->begin (), scripts->end ());
      scripts->clear ();
    }
  else
    {
      if (!down_p)
	{
	  warning (_ ("no placement found for fingerings"));
	  warning (_ ("placing below"));
	}
      down.insert (down.end (), scripts->begin (), scripts->end ());
      scripts->clear ();
    }

  for (vsize i = 0; i < horiz.size (); i++)
    {
      Finger_tuple ft = horiz[i];
      Grob *f = ft.script_;
      f->set_parent (ft.head_, X_AXIS);
      f->set_parent (ft.head_, Y_AXIS);
      f->set_property ("avoid-slur", SCM_BOOL_F);
      if (hordir == LEFT
	  && unsmob_grob (ft.head_->get_object ("accidental-grob")))
	Side_position_interface::add_support (f,
					      unsmob_grob (ft.head_->get_object ("accidental-grob")));
					      
      Self_alignment_interface::set_align_self (f, Y_AXIS);
      Self_alignment_interface::set_center_parent (f, Y_AXIS);
      Side_position_interface::set_axis (f, X_AXIS);

      f->set_property ("direction", scm_from_int (hordir));
    }

  Direction d = DOWN;
  Drul_array< vector<Finger_tuple> > vertical (down, up);
  do
    {
      for (vsize i = 0; i < vertical[d].size (); i++)
	{
	  Finger_tuple ft = vertical[d][i];
	  Grob *f = ft.script_;
	  int finger_prio = robust_scm2int (f->get_property ("script-priority"), 200);
	  f->set_parent (ft.head_, X_AXIS);
	  f->set_property ("script-priority",
			   scm_from_int (finger_prio + d * ft.position_));

	  Self_alignment_interface::set_align_self (f, X_AXIS);
	  Self_alignment_interface::set_center_parent (f, X_AXIS);
	  Side_position_interface::set_axis (f, Y_AXIS);
      
	  f->set_property ("direction", scm_from_int (d));
	}
    }
  while (flip (&d) != DOWN);
}

void
New_fingering_engraver::stop_translation_timestep ()
{
  position_all ();
  stem_ = 0;
  heads_.clear ();
}


void
New_fingering_engraver::position_all ()
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

  if (stroke_fingerings_.size ())
    {
      position_scripts (get_property ("strokeFingerOrientations"),
			&stroke_fingerings_);
      stroke_fingerings_.clear ();
    }
  
  for (vsize i = articulations_.size (); i--;)
    {
      Grob *script = articulations_[i].script_;

      for (vsize j = heads_.size (); j--;)
	Side_position_interface::add_support (script, heads_[j]);

      if (stem_ && to_dir (script->get_property ("side-relative-direction")))
	script->set_object ("direction-source", stem_->self_scm ());

      if (stem_ && to_boolean (script->get_property ("add-stem-support")))
	Side_position_interface::add_support (script, stem_);
    }
  articulations_.clear ();
}

New_fingering_engraver::New_fingering_engraver ()
{
  stem_ = 0;
}


ADD_ACKNOWLEDGER (New_fingering_engraver, rhythmic_head);
ADD_ACKNOWLEDGER (New_fingering_engraver, stem);

ADD_TRANSLATOR (New_fingering_engraver,
		/* doc */
		"Create fingering scripts for notes in a new chord.  This"
		" engraver is ill-named, since it also takes care of"
		" articulations and harmonic note heads.",

		/* create */
		"Fingering "
		"StringNumber "
		"StrokeFinger "
		"Script ",

		/* read */
		"fingeringOrientations "
		"harmonicDots "
		"strokeFingerOrientations "
		"stringNumberOrientations ",
		
		/* write */
		""
		);
