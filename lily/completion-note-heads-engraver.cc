/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "tie-column.hh"
#include "warn.hh"
#include "misc.hh"

#include "translator.icc"

#include <cctype>

using std::vector;

/*
  How does this work?

  When we catch the note, we predict the end of the note. We keep the
  events living until we reach the predicted end-time.

  Every time process_music () is called and there are note events, we
  figure out how long the note to typeset should be. It should be no
  longer than what's specified, than what is left to do and it should
  not cross barlines or sub-bar units.

  We copy the events into scratch note events, to make sure that we get
  all durations exactly right.
*/

class Completion_heads_engraver : public Engraver
{
  vector<Item *> notes_;
  vector<Item *> prev_notes_;
  // Must remember notes for explicit ties.
  vector<Spanner *> ties_;
  vector<Stream_event *> note_events_;
  Spanner *tie_column_;
  Moment note_end_mom_;
  bool is_first_;
  Rational left_to_do_;
  Rational do_nothing_until_;
  Rational factor_;

  Rational next_moment (Rational const &);
  Item *make_note_head (Stream_event *);

public:
  TRANSLATOR_DECLARATIONS (Completion_heads_engraver);

protected:
  void initialize () override;
  void make_tie (Grob *, Grob *);
  void start_translation_timestep ();
  void process_music ();
  void stop_translation_timestep ();
  void listen_note (Stream_event *);
};

void
Completion_heads_engraver::initialize ()
{
  is_first_ = false;
}

void
Completion_heads_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);

  is_first_ = true;
  auto now = now_mom ();
  auto musiclen = get_event_length (ev, now);

  note_end_mom_ = std::max (note_end_mom_, (now + musiclen));
  do_nothing_until_ = 0;
}

/*
  The duration _until_ the next bar line or completion unit
*/
Rational
Completion_heads_engraver::next_moment (Rational const &note_len)
{
  // TODO: This looks like a copy & paste from Completion_rest_engraver.
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
Completion_heads_engraver::make_note_head (Stream_event *ev)
{
  Item *note = make_item ("NoteHead", ev->self_scm ());
  Pitch *pit = unsmob<Pitch> (get_property (ev, "pitch"));

  int pos = pit ? pit->steps () : 0;
  SCM c0 = get_property (this, "middleCPosition");
  if (scm_is_number (c0))
    pos += from_scm<int> (c0);

  set_property (note, "staff-position", to_scm (pos));

  return note;
}

void
Completion_heads_engraver::process_music ()
{
  if (!is_first_ && !left_to_do_)
    return;

  is_first_ = false;

  const auto &now = now_mom ();
  if (do_nothing_until_ > now.main_part_)
    return;

  Duration note_dur;
  Duration *orig = 0;
  if (left_to_do_)
    {
      /*
        note that note_dur may be strictly less than left_to_do_
        (say, if left_to_do_ == 5/8)
      */
      note_dur = Duration (left_to_do_ / factor_, false).compressed (factor_);
    }
  else
    {
      orig = unsmob<Duration> (get_property (note_events_[0], "duration"));
      note_dur = *orig;
      SCM factor = get_property (this, "completionFactor");
      if (ly_is_procedure (factor))
        factor
          = ly_call (factor, context ()->self_scm (), note_dur.smobbed_copy ());
      factor_ = from_scm<Rational> (factor, note_dur.factor ());
      left_to_do_ = orig->get_length ();
    }

  if (const auto &nb = next_moment (note_dur.get_length ()))
    {
      if (nb < note_dur.get_length ())
        note_dur = Duration (nb / factor_, false).compressed (factor_);
    }

  do_nothing_until_ = now.main_part_ + note_dur.get_length ();

  for (vsize i = 0; left_to_do_ && i < note_events_.size (); i++)
    {
      bool need_clone = !orig || *orig != note_dur;
      Stream_event *event = note_events_[i];

      if (need_clone)
        event = event->clone ();

      SCM pits = get_property (note_events_[i], "pitch");
      set_property (event, "pitch", pits);
      set_property (event, "duration", note_dur.smobbed_copy ());
      set_property (event, "length",
                    Moment (note_dur.get_length ()).smobbed_copy ());
      set_property (event, "duration-log", to_scm (note_dur.duration_log ()));

      /*
        The Completion_heads_engraver splits an event into a group of consecutive events.
        For each event in the group, the property "autosplit-end" denotes whether the current event
        was truncated during splitting. Based on "autosplit-end", the Tie_engraver decides whether a
        tie event should be processed.
      */
      set_property (event, "autosplit-end",
                    to_scm (left_to_do_ > note_dur.get_length ()));

      Item *note = make_note_head (event);
      if (need_clone)
        event->unprotect ();
      notes_.push_back (note);
    }

  if (prev_notes_.size () == notes_.size ())
    {
      for (vsize i = 0; i < notes_.size (); i++)
        make_tie (prev_notes_[i], notes_[i]);
    }

  if (ties_.size () && !tie_column_)
    tie_column_ = make_spanner ("TieColumn", ties_[0]->self_scm ());

  if (tie_column_)
    for (vsize i = ties_.size (); i--;)
      Tie_column::add_tie (tie_column_, ties_[i]);

  left_to_do_ -= note_dur.get_length ();
  if (left_to_do_)
    {
      find_global_context ()->add_moment_to_process (
        Moment (now.main_part_ + note_dur.get_length ()));
    }
  /*
    don't do complicated arithmetic with grace notes.
  */
  if (orig && now.grace_part_)
    left_to_do_ = 0;
}

void
Completion_heads_engraver::make_tie (Grob *left, Grob *right)
{
  Spanner *p = make_spanner ("Tie", SCM_EOL);
  Tie::set_head (p, LEFT, left);
  Tie::set_head (p, RIGHT, right);
  announce_end_grob (p, SCM_EOL);
  ties_.push_back (p);
}

void
Completion_heads_engraver::stop_translation_timestep ()
{
  ties_.clear ();
  tie_column_ = 0;

  if (notes_.size ())
    prev_notes_ = notes_;
  notes_.clear ();
}

void
Completion_heads_engraver::start_translation_timestep ()
{
  auto now = now_mom ();
  if (note_end_mom_.main_part_ <= now.main_part_)
    {
      note_events_.clear ();
      prev_notes_.clear ();
    }
  set_property (context (), "completionBusy", to_scm (!note_events_.empty ()));
}

Completion_heads_engraver::Completion_heads_engraver (Context *c)
  : Engraver (c)
{
  tie_column_ = 0;
}

void
Completion_heads_engraver::boot ()
{
  ADD_LISTENER (note);
}

ADD_TRANSLATOR (Completion_heads_engraver,
                /* doc */
                R"(
This engraver replaces @code{Note_heads_engraver}.  It plays some trickery to
break long notes and automatically tie them into the next measure.
                )",

                /* create */
                R"(
NoteHead
Tie
TieColumn
                )",

                /* read */
                R"(
completionFactor
completionUnit
measureLength
measurePosition
middleCPosition
timing
                )",

                /* write */
                R"(
completionBusy
                )");
