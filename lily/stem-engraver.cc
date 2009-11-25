/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "engraver.hh"

#include "context.hh"
#include "directional-element-interface.hh"
#include "duration.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "rhythmic-head.hh"
#include "script-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "stem.hh"
#include "stream-event.hh"

#include "translator.icc"

class Stem_engraver : public Engraver
{
  Grob *stem_;
  Grob *tremolo_;
  Stream_event *rhythmic_ev_;
  Stream_event *tremolo_ev_;

  TRANSLATOR_DECLARATIONS (Stem_engraver);

protected:
  void make_stem (Grob_info);

  DECLARE_TRANSLATOR_LISTENER (tremolo);
  DECLARE_ACKNOWLEDGER (rhythmic_head);
  void stop_translation_timestep ();
};

Stem_engraver::Stem_engraver ()
{
  tremolo_ev_ = 0;
  stem_ = 0;
  tremolo_ = 0;
  rhythmic_ev_ = 0;
}

void
Stem_engraver::make_stem (Grob_info gi)
{
  /* Announce the cause of the head as cause of the stem.  The
     stem needs a rhythmic structure to fit it into a beam.  */
  stem_ = make_item ("Stem", gi.grob ()->self_scm ());

  if (tremolo_ev_)
    {
      /* Stem tremolo is never applied to a note by default,
	 is must me requested.  But there is a default for the
	 tremolo value:

	 c4:8 c c:

	 the first and last (quarter) note bothe get one tremolo flag.  */
      int requested_type
	= scm_to_int (tremolo_ev_->get_property ("tremolo-type"));
      SCM f = get_property ("tremoloFlags");
      if (!requested_type)
	{
	  if (scm_is_number (f))
	    requested_type = scm_to_int (f);
	  else
	    requested_type = 8;
	}
      else
	context ()->set_property ("tremoloFlags", scm_from_int (requested_type));


      /*
	we take the duration log from the Event, since the duration-log
	for a note head is always <= 2.
      */
      Stream_event *ev = gi.event_cause ();
      Duration *dur = unsmob_duration (ev->get_property ("duration"));
      
      int tremolo_flags = intlog2 (requested_type) - 2
	- (dur->duration_log () > 2 ? dur->duration_log () - 2 : 0);
      if (tremolo_flags <= 0)
	{
	  tremolo_ev_->origin ()->warning (_ ("tremolo duration is too long"));
	  tremolo_flags = 0;
	}

      if (tremolo_flags)
	{
	  tremolo_ = make_item ("StemTremolo", tremolo_ev_->self_scm ());

	  /* The number of tremolo flags is the number of flags of the
	     tremolo-type minus the number of flags of the note itself.  */
	  tremolo_->set_property ("flag-count", scm_from_int (tremolo_flags));
	  tremolo_->set_parent (stem_, X_AXIS);
	  stem_->set_object ("tremolo-flag", tremolo_->self_scm ());
	  tremolo_->set_object ("stem", stem_->self_scm ());
	}
    }
}

void
Stem_engraver::acknowledge_rhythmic_head (Grob_info gi)
{
  if (Rhythmic_head::get_stem (gi.grob ()))
    return;

  Stream_event *cause = gi.event_cause ();
  if (!cause)
    return;
  Duration *d = unsmob_duration (cause->get_property ("duration"));
  if (!d)
    return;

  if (!stem_)
    make_stem (gi);

  if (Stem::duration_log (stem_) != d->duration_log ())
    {
      // FIXME: 
      gi.event_cause ()->origin ()->warning (_f ("adding note head to incompatible stem (type = %d)",
						 1 << Stem::duration_log (stem_)));
      gi.event_cause ()->origin ()->warning (_ ("maybe input should specify polyphonic voices"));
    }

  Stem::add_head (stem_, gi.grob ());
}

void
Stem_engraver::stop_translation_timestep ()
{
  tremolo_ = 0;
  if (stem_)
    {
      /* FIXME: junk these properties.  */
      SCM prop = get_property ("stemLeftBeamCount");
      if (scm_is_number (prop))
	{
	  Stem::set_beaming (stem_, scm_to_int (prop), LEFT);
	  context ()->unset_property (ly_symbol2scm ("stemLeftBeamCount"));
	}
      prop = get_property ("stemRightBeamCount");
      if (scm_is_number (prop))
	{
	  Stem::set_beaming (stem_, scm_to_int (prop), RIGHT);
	  context ()->unset_property (ly_symbol2scm ("stemRightBeamCount"));
	}
      stem_ = 0;
    }
  tremolo_ev_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Stem_engraver, tremolo);
void
Stem_engraver::listen_tremolo (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (tremolo_ev_, ev);
}

ADD_ACKNOWLEDGER (Stem_engraver, rhythmic_head);

ADD_TRANSLATOR (Stem_engraver,
		/* doc */
		"Create stems and single-stem tremolos.  It also works"
		" together with the beam engraver for overriding beaming.",

		/* create */
		"Stem "
		"StemTremolo ",

		/* read */
		"tremoloFlags "
		"stemLeftBeamCount "
		"stemRightBeamCount ",

		/* write */
		""
		);
