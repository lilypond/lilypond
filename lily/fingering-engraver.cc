/*
  fingering-engraver.cc -- implement Fingering_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "item.hh"

#include "translator.icc"

class Fingering_engraver : public Engraver
{
  vector<Stream_event*> events_;
  vector<Item*> fingerings_;

public:
  TRANSLATOR_DECLARATIONS (Fingering_engraver);
protected:
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (fingering);
  DECLARE_TRANSLATOR_LISTENER (stroke_finger);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  DECLARE_ACKNOWLEDGER (stem);

private:
  void make_script (Direction, Stream_event *, int);
};

IMPLEMENT_TRANSLATOR_LISTENER (Fingering_engraver, fingering);
void
Fingering_engraver::listen_fingering (Stream_event *ev)
{
  events_.push_back (ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Fingering_engraver, stroke_finger);
void
Fingering_engraver::listen_stroke_finger (Stream_event * /* ev */)
{
  /*
    FIXME: should do something.
    
    This function is mainly here to shut up a warning
   */
}

void
Fingering_engraver::acknowledge_stem (Grob_info inf)
{
  for (vsize i = 0; i < fingerings_.size (); i++)
    Side_position_interface::add_support (fingerings_[i], inf.grob ());
}

void
Fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  for (vsize i = 0; i < fingerings_.size (); i++)
    {
      Grob *t = fingerings_[i];
      Side_position_interface::add_support (t, inf.grob ());
      if (!t->get_parent (X_AXIS))
	t->set_parent (inf.grob (), X_AXIS);
    }
}

void
Fingering_engraver::process_music ()
{
  for (vsize i = events_.size (); i--;)
    {
      SCM dir = events_[i]->get_property ("direction");
      make_script (to_dir (dir), events_[i], i);
    }
}

void
Fingering_engraver::make_script (Direction d, Stream_event *r, int i)
{
  Item *fingering = make_item ("Fingering", r->self_scm ());

  /*
    Huh, what's this for? --hwn.

    junkme.
  */
  SCM pitch = r->get_property ("pitch");
  if (unsmob_pitch (pitch))
    fingering->set_property ("pitch", pitch);

  /*
    We can't fold these definitions into define-grobs since
    fingerings for chords need different settings.
  */
  Side_position_interface::set_axis (fingering, Y_AXIS);
  Self_alignment_interface::set_align_self (fingering, X_AXIS);
  Self_alignment_interface::set_center_parent (fingering, X_AXIS);

  // Hmm
  int priority = 200;
  SCM s = fingering->get_property ("script-priority");
  if (scm_is_number (s))
    priority = scm_to_int (s);

  /* See script-engraver.cc */
  priority += i;

  fingering->set_property ("script-priority", scm_from_int (priority));

  if (!is_direction (fingering->get_property_data ("direction")))
    {
      if (d)
	fingering->set_property ("direction", scm_from_int (d));
      else
	fingering->set_property ("direction", scm_from_int (RIGHT));
    }

  fingerings_.push_back (fingering);
}

void
Fingering_engraver::stop_translation_timestep ()
{
  if (!fingerings_.size ())
    return;

  fingerings_.clear ();
  events_.clear ();
}

Fingering_engraver::Fingering_engraver ()
{
}

ADD_ACKNOWLEDGER (Fingering_engraver, rhythmic_head);
ADD_ACKNOWLEDGER (Fingering_engraver, stem);

ADD_TRANSLATOR (Fingering_engraver,
		/* doc */
		"Create fingering scripts.",

		/* create */
		"Fingering ",

		/* read */
		"",

		/* write */
		""
		);
