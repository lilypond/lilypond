/*
  chord-tremolo-engraver.cc -- implement Chord_tremolo_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "beam.hh"
#include "repeated-music.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "engraver-group.hh"
#include "warn.hh"
#include "misc.hh"
#include "spanner.hh"
#include "item.hh"
#include "chord-tremolo-iterator.hh"
#include "stem-tremolo.hh"
#include "math.h" // ceil

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
  void typeset_beam ();
  TRANSLATOR_DECLARATIONS (Chord_tremolo_engraver);
protected:
  Music *repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;
  int flags_;
  int total_duration_flags_;

  /// location  within measure where beam started.
  Moment beam_start_location_;

  bool body_is_sequential_;
  Spanner *beam_;
  Spanner *finished_beam_;
  Item *stem_tremolo_;
protected:
  virtual void finalize ();
  virtual bool try_music (Music *);
  void stop_translation_timestep ();
  void start_translation_timestep ();
  void process_music ();
  DECLARE_ACKNOWLEDGER (stem);
};

Chord_tremolo_engraver::Chord_tremolo_engraver ()
{
  beam_ = finished_beam_ = 0;
  repeat_ = 0;
  flags_ = 0;
  stem_tremolo_ = 0;
  body_is_sequential_ = false;
}

bool
Chord_tremolo_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("repeated-music")
      && m->get_property ("iterator-ctor") == Chord_tremolo_iterator::constructor_proc
      && !repeat_)
    {
      Moment l = m->get_length ();
      repeat_ = m;
      start_mom_ = now_mom ();
      stop_mom_ = start_mom_ + l;

      Music *body = Repeated_music::body (m);
      body_is_sequential_ = body->is_mus_type ("sequential-music");

      int elt_count = body_is_sequential_ ? scm_ilength (body->get_property ("elements")) : 1;

      if (body_is_sequential_ && elt_count != 2)
	m->origin ()->warning (_f ("expect 2 elements for chord tremolo, found %d", elt_count));

      if (elt_count <= 0)
	elt_count = 1;

      Rational total_dur = l.main_part_;
      Rational note_dur = total_dur / Rational (elt_count * Repeated_music::repeat_count (repeat_));

      total_duration_flags_ = max (0, (intlog2 (total_dur.den ()) - 2));

      flags_ = intlog2 (note_dur.den ()) -2;

      return true;
    }

  return false;
}

void
Chord_tremolo_engraver::process_music ()
{
  if (repeat_ && body_is_sequential_ && !beam_)
    {
      beam_ = make_spanner ("Beam", repeat_->self_scm ());
      beam_->set_property ("chord-tremolo", SCM_BOOL_T);

      beam_start_location_ = robust_scm2moment (get_property ("measurePosition"), Moment (0));
    }
}

void
Chord_tremolo_engraver::finalize ()
{
  typeset_beam ();
  if (beam_)
    {
      repeat_->origin ()->warning (_ ("unterminated chord tremolo"));
      beam_->suicide ();
    }
}

void
Chord_tremolo_engraver::typeset_beam ()
{
  finished_beam_ = 0;
}

void
Chord_tremolo_engraver::acknowledge_stem (Grob_info info)
{
  if (beam_)
    {
      Grob *s = info.grob ();

      if (start_mom_ == now_mom ())
	Stem::set_beaming (s, flags_, RIGHT);
      else
	Stem::set_beaming (s, flags_, LEFT);

      if (Stem::duration_log (s) != 1)
	beam_->set_property ("gap-count", scm_from_int (flags_ - total_duration_flags_));

      if (info.ultimate_music_cause ()->is_mus_type ("rhythmic-event"))
	Beam::add_stem (beam_, s);
      else
	{
	  String s = _ ("stem must have Rhythmic structure");
	  if (info.music_cause ())
	    info.music_cause ()->origin ()->warning (s);
	  else
	    ::warning (s);
	}
    }
  else if (repeat_
	   && flags_
	   && !body_is_sequential_)
    {
      stem_tremolo_ = make_item ("StemTremolo", repeat_->self_scm ());
      stem_tremolo_->set_property ("flag-count",
				   scm_from_int (flags_));
      stem_tremolo_->set_object ("stem",
				 info.grob ()->self_scm ());
      stem_tremolo_->set_parent (info.grob (), X_AXIS);
    }
}

void
Chord_tremolo_engraver::start_translation_timestep ()
{
  if (beam_ && stop_mom_ == now_mom ())
    {
      finished_beam_ = beam_;
      repeat_ = 0;
      beam_ = 0;
    }
}

void
Chord_tremolo_engraver::stop_translation_timestep ()
{
  if (stem_tremolo_)
    {
      repeat_ = 0;
      if (beam_)
	programming_error ("beam and stem tremolo?");
      stem_tremolo_ = 0;
    }

  typeset_beam ();
}

ADD_ACKNOWLEDGER (Chord_tremolo_engraver, stem);
ADD_TRANSLATOR (Chord_tremolo_engraver,
		/* doc */ "Generates beams for  tremolo repeats.",
		/* create */ "Beam",
		/* accept */ "repeated-music",
		/* read */ "",
		/* write */ "");
