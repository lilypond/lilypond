/*
  slash-repeat-engraver.cc -- implement Slash_repeat_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2000--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "repeated-music.hh"
#include "global-context.hh"
#include "warn.hh"
#include "misc.hh"
#include "spanner.hh"
#include "item.hh"
#include "percent-repeat-iterator.hh"
#include "bar-line.hh"
#include "score-engraver.hh"

/**
   This acknowledges repeated music with "percent" style.  It typesets
   a % sign.

   TODO:

   - BEAT case: Create items for single beat repeats, i.e. c4 / / /

   - DOUBLE_MEASURE case: attach a % to an appropriate barline.
*/
class Slash_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Slash_repeat_engraver);
protected:
  Music *repeat_;

  /// moment (global time) where beam started.
  Moment start_mom_;
  Moment stop_mom_;

  /// location  within measure where beam started.
  Moment beam_start_location_;
  Moment next_moment_;
  Moment body_length_;

  Item *beat_slash_;
  Item *double_percent_;
protected:
  virtual bool try_music (Music *);
  void start_translation_timestep ();
  void process_music ();
};

Slash_repeat_engraver::Slash_repeat_engraver ()
{
  repeat_ = 0;
  beat_slash_ = 0;
}

bool
Slash_repeat_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("repeated-music")
      && !repeat_
      && m->get_property ("iterator-ctor")
      == Percent_repeat_iterator::constructor_proc)
    {
      body_length_ = Repeated_music::body_get_length (m);
      int count = Repeated_music::repeat_count (m);

      Moment now = now_mom ();
      start_mom_ = now;
      stop_mom_ = start_mom_ + Moment (count) * body_length_;
      next_moment_ = start_mom_ + body_length_;

      Moment meas_length
	= robust_scm2moment (get_property ("measureLength"), Moment (0));
      if (body_length_ < meas_length)
	repeat_ = m;
      else
	return false;

      Global_context *global = get_global_context ();
      for (int i = 0; i < count; i++)
	global->add_moment_to_process (next_moment_ + Moment (i) * body_length_);

      return true;
    }

  return false;
}

void
Slash_repeat_engraver::process_music ()
{
  if (repeat_ && now_mom () == next_moment_)
    {
      beat_slash_ = make_item ("RepeatSlash", repeat_->self_scm ());
      next_moment_ = next_moment_ + body_length_;

      get_global_context ()->add_moment_to_process (next_moment_);
    }
}

void
Slash_repeat_engraver::start_translation_timestep ()
{
  if (stop_mom_ == now_mom ())
    repeat_ = 0;
  beat_slash_ = 0;
}

#include "translator.icc"

ADD_TRANSLATOR (Slash_repeat_engraver,
		/* doc */ "Make beat repeats.",
		/* create */ "RepeatSlash",
		/* accept */ "repeated-music",
		/* read */ "measureLength",
		/* write */ "");
