/*
  chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
  		 Erik Sandberg <mandolaerik@gmail.com>
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

  int flags_;
  // number of beams for short tremolos
  int expected_beam_count_;
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
  flags_ = 0;
  expected_beam_count_ = 0;
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
	  int type = scm_to_int (ev->get_property ("tremolo-type"));
	  /* e.g. 1 for type 8, 2 for type 16 */
	  flags_ = intlog2 (type) - 2;
	  expected_beam_count_ = scm_to_int (ev->get_property ("expected-beam-count"));
	  beam_dir_ = RIGHT;
	}
    }
  else if (span_dir == STOP)
    {
      if (!repeat_)
	ev->origin ()->warning (_ ("No tremolo to end"));
      repeat_ = 0;
      beam_ = 0;
      expected_beam_count_ = 0;
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
      Grob *s = info.grob ();

      Stem::set_beaming (s, flags_, beam_dir_);

      if (Stem::duration_log (s) != 1)
	beam_->set_property ("gap-count", scm_from_int (flags_ - expected_beam_count_));

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
