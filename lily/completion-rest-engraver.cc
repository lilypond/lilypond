/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
                           Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "dot-column.hh"
#include "duration.hh"
#include "global-context.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "score-engraver.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "tie.hh"
#include "warn.hh"
#include "misc.hh"

#include "translator.icc"

#include <cctype>

using std::vector;

/*
  How does this work?

  When we catch the rest, we predict the end of the rest. We keep the
  events living until we reach the predicted end-time.

  Every time process_music () is called and there are rest events, we
  figure out how long the rest to typeset should be. It should be no
  longer than what's specified, than what is left to do and it should
  not cross barlines or sub-bar units.

  We copy the events into scratch rest events, to make sure that we get
  all durations exactly right.
*/

class Completion_rest_engraver : public Engraver
{
  vector<Item *> rests_;
  vector<Stream_event *> rest_events_;
  Moment rest_end_mom_;
  bool is_first_;
  Rational left_to_do_;
  Rational do_nothing_until_;
  Rational factor_;

  Rational next_moment (Rational const &);
  Item *make_rest (Stream_event *);

public:
  TRANSLATOR_DECLARATIONS (Completion_rest_engraver);

protected:
  void initialize () override;
  void start_translation_timestep ();
  void process_music ();
  void stop_translation_timestep ();
  void listen_rest (Stream_event *);
};

void
Completion_rest_engraver::initialize ()
{
  is_first_ = false;
}

void
Completion_rest_engraver::listen_rest (Stream_event *ev)
{
  rest_events_.push_back (ev);

  is_first_ = true;
  auto now = now_mom ();
  auto musiclen = get_event_length (ev, now);

  rest_end_mom_ = std::max (rest_end_mom_, (now + musiclen));
  do_nothing_until_ = 0;
}

/*
  The duration _until_ the next barline or completion unit
*/
Rational
Completion_rest_engraver::next_moment (Rational const &note_len)
{
  // TODO: This looks like a copy & paste from Completion_heads_engraver.
  Rational result;

  if (!from_scm<bool> (get_property (this, "timing")))
    {
      return result;
    }

  auto *const mpos = unsmob<Moment> (get_property (this, "measurePosition"));
  auto *const mlen = unsmob<Moment> (get_property (this, "measureLength"));
  if (!mpos || !mlen)
    {
      return result; // programming error?
    }

  if (*mpos > *mlen)
    {
      programming_error ("invalid measure position: " + to_string (*mpos)
                         + " of " + to_string (*mlen));
      return result;
    }

  result = mlen->main_part_ - mpos->main_part_;

  Rational unit;
  if (auto *const u = unsmob<Moment> (get_property (this, "completionUnit")))
    {
      unit = u->main_part_;
    }
  else
    {
      return result;
    }

  Rational const now_unit = mpos->main_part_ / unit;
  if (now_unit.den () > 1)
    {
      /*
        within a unit - go to the end of that
      */
      result = unit * (Rational (1) - (now_unit - now_unit.trunc_rat ()));
    }
  else
    {
      /*
        at the beginning of a unit:
        take a power-of-two number of units, but not more than required,
        since then the Duration constructor destroys the unit structure
      */
      if (note_len < result)
        result = note_len;
      Rational const step_unit = result / unit;
      if (step_unit.den () < step_unit.num ())
        {
          int const log2 = intlog2 (int (step_unit.num () / step_unit.den ()));
          result = unit * Rational (1 << log2);
        }
    }

  return result;
}

Item *
Completion_rest_engraver::make_rest (Stream_event *ev)
{
  Item *rest = make_item ("Rest", ev->self_scm ());
  if (Pitch *p = unsmob<Pitch> (get_property (ev, "pitch")))
    {
      int pos = p->steps ();
      SCM c0 = get_property (this, "middleCPosition");
      if (scm_is_number (c0))
        pos += from_scm<int> (c0);
      set_property (rest, "staff-position", to_scm (pos));
    }

  return rest;
}

void
Completion_rest_engraver::process_music ()
{
  if (!is_first_ && !left_to_do_)
    return;

  is_first_ = false;

  const auto &now = now_mom ();
  if (do_nothing_until_ > now.main_part_)
    return;

  Duration rest_dur;
  Duration *orig = 0;
  if (left_to_do_)
    {
      /*
        note that rest_dur may be strictly less than left_to_do_
        (say, if left_to_do_ == 5/8)
      */
      rest_dur = Duration (left_to_do_ / factor_, false).compressed (factor_);
    }
  else
    {
      orig = unsmob<Duration> (get_property (rest_events_[0], "duration"));
      rest_dur = *orig;
      SCM factor = get_property (this, "completionFactor");
      if (ly_is_procedure (factor))
        factor
          = ly_call (factor, context ()->self_scm (), rest_dur.smobbed_copy ());
      factor_ = from_scm<Rational> (factor, rest_dur.factor ());
      left_to_do_ = orig->get_length ();
    }

  if (const auto &nb = next_moment (rest_dur.get_length ()))
    {
      if (nb < rest_dur.get_length ())
        rest_dur = Duration (nb / factor_, false).compressed (factor_);
    }

  do_nothing_until_ = now.main_part_ + rest_dur.get_length ();

  for (vsize i = 0; left_to_do_ && i < rest_events_.size (); i++)
    {
      bool need_clone = !orig || *orig != rest_dur;
      Stream_event *event = rest_events_[i];

      if (need_clone)
        event = event->clone ();

      SCM pits = get_property (rest_events_[i], "pitch");
      set_property (event, "pitch", pits);
      set_property (event, "duration", rest_dur.smobbed_copy ());
      set_property (event, "length",
                    Moment (rest_dur.get_length ()).smobbed_copy ());
      set_property (event, "duration-log", to_scm (rest_dur.duration_log ()));

      Item *rest = make_rest (event);
      if (need_clone)
        event->unprotect ();
      rests_.push_back (rest);
    }

  left_to_do_ -= rest_dur.get_length ();
  if (left_to_do_)
    {
      find_global_context ()->add_moment_to_process (
        Moment (now.main_part_ + rest_dur.get_length ()));
    }
  /*
    don't do complicated arithmetic with grace rests.
  */
  if (orig && now.grace_part_)
    left_to_do_ = 0;
}

void
Completion_rest_engraver::stop_translation_timestep ()
{
  rests_.clear ();
}

void
Completion_rest_engraver::start_translation_timestep ()
{
  auto now = now_mom ();
  if (rest_end_mom_.main_part_ <= now.main_part_)
    {
      rest_events_.clear ();
    }
  set_property (context (), "restCompletionBusy",
                to_scm (!rest_events_.empty ()));
}

Completion_rest_engraver::Completion_rest_engraver (Context *c)
  : Engraver (c)
{
}

void
Completion_rest_engraver::boot ()
{
  ADD_LISTENER (rest);
}

ADD_TRANSLATOR (Completion_rest_engraver,
                /* doc */
                R"(
This engraver replaces @code{Rest_engraver}.  It plays some trickery to break
long rests into the next measure.
                )",

                /* create */
                R"(
Rest
                )",

                /* read */
                R"(
completionFactor
completionUnit
middleCPosition
measurePosition
measureLength
                )",

                /* write */
                R"(
restCompletionBusy
                )");
