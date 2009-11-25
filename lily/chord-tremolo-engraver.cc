/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  		 Erik Sandberg <mandolaerik@gmail.com>

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

#include "beam.hh"
#include "engraver-group.hh"
#include "international.hh"
#include "item.hh"
#include "math.h" // ceil
#include "misc.hh"
#include "repeated-music.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stem-tremolo.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/**

This acknowledges repeated music with "tremolo" style.  It typesets
a beam.

TODO:

- perhaps use engraver this to steer other engravers? That would
create dependencies between engravers, which is bad.

- create dots if appropriate.

- create TremoloBeam iso Beam?
*/
class Chord_tremolo_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Chord_tremolo_engraver);
protected:
  Stream_event *repeat_;

  // current direction of beam (first RIGHT, then LEFT)
  Direction beam_dir_;

  Spanner *beam_;

protected:
  virtual void finalize ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (tremolo_span);
  DECLARE_ACKNOWLEDGER (stem);
};

Chord_tremolo_engraver::Chord_tremolo_engraver ()
{
  beam_ = 0;
  repeat_ = 0;
  beam_dir_ = CENTER;
}

IMPLEMENT_TRANSLATOR_LISTENER (Chord_tremolo_engraver, tremolo_span);
void
Chord_tremolo_engraver::listen_tremolo_span (Stream_event *ev)
{
  Direction span_dir = to_dir (ev->get_property ("span-direction"));
  if (span_dir == START)
    {
      if (ASSIGN_EVENT_ONCE (repeat_, ev))
	{
	  beam_dir_ = RIGHT;
	}
    }
  else if (span_dir == STOP)
    {
      if (!repeat_)
	ev->origin ()->warning (_ ("No tremolo to end"));
      repeat_ = 0;
      beam_ = 0;
      beam_dir_ = CENTER;
    }
}

void
Chord_tremolo_engraver::process_music ()
{
  if (repeat_ && !beam_)
    {
      beam_ = make_spanner ("Beam", repeat_->self_scm ());
    }
}

void
Chord_tremolo_engraver::finalize ()
{
  if (beam_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
      announce_end_grob (beam_, SCM_EOL);
      beam_->suicide ();
    }
}

void
Chord_tremolo_engraver::acknowledge_stem (Grob_info info)
{
  if (beam_)
    {
      int tremolo_type = robust_scm2int (repeat_->get_property ("tremolo-type"), 1);
      int flags = max (0, intlog2 (tremolo_type) - 2);
      int repeat_count = robust_scm2int (repeat_->get_property ("repeat-count"), 1);
      int gap_count = min (flags, intlog2 (repeat_count) + 1);

      Grob *s = info.grob ();
      Stem::set_beaming (s, flags, beam_dir_);

      if (Stem::duration_log (s) != 1)
	beam_->set_property ("gap-count", scm_from_int (gap_count));

      if (beam_dir_ == RIGHT)
	{
	  beam_dir_ = LEFT;
	  announce_end_grob (beam_, s->self_scm ());
	}
      
      if (info.ultimate_event_cause ()->in_event_class ("rhythmic-event"))
	Beam::add_stem (beam_, s);
      else
	{
	  string s = _ ("stem must have Rhythmic structure");
	  if (info.event_cause ())
	    info.event_cause ()->origin ()->warning (s);
	  else
	    ::warning (s);
	}
    }
}

ADD_ACKNOWLEDGER (Chord_tremolo_engraver, stem);
ADD_TRANSLATOR (Chord_tremolo_engraver,
		/* doc */
		"Generate beams for tremolo repeats.",

		/* create */
		"Beam ",

		/* read */
		"",

		/* write */
		""
		);
