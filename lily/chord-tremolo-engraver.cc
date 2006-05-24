/*
  chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  		 Erik Sandberg <mandolaerik@gmail.com>
*/

#include "math.h" // ceil

#include "beam.hh"
#include "engraver-group.hh"
#include "international.hh"
#include "item.hh"
#include "misc.hh"
#include "repeated-music.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stem-tremolo.hh"
#include "stem.hh"
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
  Music *repeat_;

  int flags_;
  // number of beams for short tremolos
  int expected_beam_count_;
  // current direction of beam (first RIGHT, then LEFT)
  Direction beam_dir_;

  Spanner *beam_;
protected:
  virtual void finalize ();
  virtual bool try_music (Music *);
  void process_music ();
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

bool
Chord_tremolo_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("tremolo-span-event"))
    {
      Direction span_dir = to_dir (m->get_property ("span-direction"));
      if (span_dir == START)
	{
	  repeat_ = m;
	  int type = scm_to_int (m->get_property ("tremolo-type"));
	  /* e.g. 1 for type 8, 2 for type 16 */
	  flags_ = intlog2 (type) - 2;
	  expected_beam_count_ = scm_to_int (m->get_property ("expected-beam-count"));
	  beam_dir_ = RIGHT;
	}
      if (span_dir == STOP)
	{
	  repeat_ = 0;
          beam_ = 0;
          expected_beam_count_ = 0;
          beam_dir_ = CENTER;
	}
      return true;
    }
  return false;
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
        beam_dir_ = LEFT;

      if (info.ultimate_music_cause ()->is_mus_type ("rhythmic-event"))
	Beam::add_stem (beam_, s);
      else
	{
	  string s = _ ("stem must have Rhythmic structure");
	  if (info.music_cause ())
	    info.music_cause ()->origin ()->warning (s);
	  else
	    ::warning (s);
	}
    }
}

ADD_ACKNOWLEDGER (Chord_tremolo_engraver, stem);
ADD_TRANSLATOR (Chord_tremolo_engraver,
		/* doc */ "Generates beams for tremolo repeats.",
		/* create */ "Beam",
		/* accept */ "tremolo-span-event",
		/* read */ "",
		/* write */ "");
