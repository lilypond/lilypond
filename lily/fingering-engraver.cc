/*
  fingering-engraver.cc -- implement Fingering_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1998--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "engraver.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "self-alignment-interface.hh"
#include "pitch.hh"


class Fingering_engraver : public Engraver
{
  Link_array<Music> events_;
  Link_array<Item> fingerings_;

public:
  TRANSLATOR_DECLARATIONS (Fingering_engraver);
protected:
  virtual bool try_music (Music *m);
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  DECLARE_ACKNOWLEDGER (stem);

private:
  void make_script (Direction, Music *, int);
};

bool
Fingering_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("fingering-event"))
    {
      events_.push (m);
      return true;
    }
  return false;
}

void
Fingering_engraver::acknowledge_stem (Grob_info inf)
{
  for (int i = 0; i < fingerings_.size (); i++)
    {
      Side_position_interface::add_support (fingerings_[i], inf.grob ());
    }
}

void
Fingering_engraver::acknowledge_rhythmic_head (Grob_info inf)
{
  for (int i = 0; i < fingerings_.size (); i++)
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
  for (int i = events_.size (); i--;)
    {
      SCM dir = events_[i]->get_property ("direction");
      make_script (to_dir (dir), events_[i], i);
    }
}

void
Fingering_engraver::make_script (Direction d, Music *r, int i)
{
  Item *fingering = make_item ("Fingering", r->self_scm ());
  Axis a = Y_AXIS;
  Axis other = other_axis (a);

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
  Side_position_interface::set_axis (fingering, a);
  fingering->add_offset_callback (Self_alignment_interface::aligned_on_self_proc, other);
  fingering->add_offset_callback (Self_alignment_interface::centered_on_parent_proc, other);

  // Hmm
  int priority = 200;
  SCM s = fingering->get_property ("script-priority");
  if (scm_is_number (s))
    priority = scm_to_int (s);

  /* See script-engraver.cc */
  priority += i;

  fingering->set_property ("script-priority", scm_from_int (priority));

  if (!is_direction (fingering->get_property ("direction")))
    {
      if (d)
	fingering->set_property ("direction", scm_from_int (d));
      else
	fingering->set_property ("direction", scm_from_int (RIGHT));
    }

  SCM dig = r->get_property ("digit");
  fingering->set_property ("text", scm_number_to_string (dig, scm_from_int (10)));

  fingerings_.push (fingering);
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

#include "translator.icc"

ADD_ACKNOWLEDGER (Fingering_engraver,rhythmic_head);
ADD_ACKNOWLEDGER (Fingering_engraver,stem);
ADD_TRANSLATOR (Fingering_engraver,
		/* descr */ "Create fingering-scripts",
		/* creats*/ "Fingering",
		/* accepts */ "fingering-event",
		/* reads */ "",
		/* write */ "");
