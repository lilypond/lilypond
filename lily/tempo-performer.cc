/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "performer.hh"

#include "audio-column.hh"
#include "audio-item.hh"
#include "duration.hh"
#include "international.hh"
#include "stream-event.hh"

#include "translator.icc"

class Tempo_performer final : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Tempo_performer);
  ~Tempo_performer ();

protected:
  void listen_tempo_change (Stream_event *);
  void listen_tempo_gradual_change (Stream_event *);
  void pre_process_music ();
  void process_music ();
  void stop_translation_timestep ();
  void finalize () override;

  void close_tempo (Moment);
  void close_span_tempo ();

private:
  Stream_event *accel_ev_ = nullptr;      // or decel.
  Stream_event *accel_stop_ev_ = nullptr; // or decel.
  Stream_event *tempo_ev_ = nullptr;
  Audio_span_tempo *span_ = nullptr;
  bool span_ramping_ = false;
  std::vector<Audio_tempo *> spanned_changes_;
  Moment last_change_mom_ = -Moment::infinity ();
  Rational wpm_ = -Rational::infinity ();
  Rational accel_stop_wpm_ = -Rational::infinity ();
  bool ramping_ = false;
};

Tempo_performer::Tempo_performer (Context *c)
  : Performer (c)
{
}

Tempo_performer::~Tempo_performer ()
{
}

void
Tempo_performer::close_tempo (Moment now)
{
  if (!spanned_changes_.empty ())
    {
      auto *const last = spanned_changes_.back ();
      if (!last->has_end_moment ())
        {
          last->set_end_moment (now);
        }
    }
}

void
Tempo_performer::close_span_tempo ()
{
  const auto &now = now_mom ();
  close_tempo (now);
  span_->set_end_moment (now);
  if (span_ramping_)
    {
      // If a TempoGradualChangeEvent provided a target tempo, use it.
      // Otherwise, use the going-forward tempo.
      span_->set_end_wpm ((accel_stop_wpm_ > 0) ? accel_stop_wpm_ : wpm_);
    }

  // If the automatically generated changes were unnecessarily frequent, this
  // would be a good time to decimate them.  A reasonable way to do it would be
  // to coalesce adjacent changes so that one's interval is expanded to cover
  // both and the other's is reduced to a point.  Midi_item::get_midi already
  // filters out the latter for robustness.

  spanned_changes_.clear ();
  span_ = nullptr;
  span_ramping_ = false;
}

void
Tempo_performer::listen_tempo_change (Stream_event *ev)
{
  assign_event_once (tempo_ev_, ev);
}

void
Tempo_performer::listen_tempo_gradual_change (Stream_event *ev)
{
  const auto d = from_scm (get_property (ev, "span-direction"), CENTER);
  if (d == START)
    assign_event_once (accel_ev_, ev);
  else if ((d == STOP) && assign_event_once (accel_stop_ev_, ev))
    {
      SCM unit_scm = get_property (accel_stop_ev_, "tempo-unit");
      SCM count_scm = get_property (accel_stop_ev_, "metronome-count");
      if (!scm_is_null (unit_scm) && !scm_is_null (count_scm))
        {
          auto unit = [&] { // TODO: from_scm<Duration>
            if (auto *dur = unsmob<Duration> (unit_scm))
              return *dur;
            return Duration (1);
          }();
          const auto count = from_scm<Rational> (count_scm);
          accel_stop_wpm_ = Rational (unit) * count;
        }
      else
        {
          if (!scm_is_null (unit_scm))
            programming_error ("tempo-unit without metronome-count");

          if (!scm_is_null (count_scm))
            programming_error ("metronome-count without tempo-unit");
        }
    }
}

void
Tempo_performer::pre_process_music ()
{
  const auto wpm
    = from_scm (get_property (this, "tempoWholesPerMinute"), wpm_);
  const bool change_wpm = (wpm != wpm_);
  if (change_wpm || accel_stop_ev_)
    {
      wpm_ = wpm;
      last_change_mom_ = now_mom ();
      ramping_ = false;
    }

  if (accel_ev_)
    {
      if (!ramping_)
        {
          last_change_mom_ = now_mom ();
          ramping_ = true;
        }
      else
        {
          accel_ev_->warning (_ ("tempo change already in progress; "
                                 "ignoring"));
          accel_ev_ = nullptr;
        }
    }

  // At a change, finalize the previous span.
  if (span_ && (span_->get_start_moment () < last_change_mom_))
    {
      close_span_tempo ();
    }

  // When part of the performance is skipped, end an ongoing Audio_tempo so
  // that it doesn't receive any contribution for the skipped music.
  if (from_scm<bool> (get_property (this, "skipTypesetting")))
    {
      close_tempo (now_mom ());
    }
}

void
Tempo_performer::process_music ()
{
  if (!span_)
    {
      auto *const cause = ramping_ ? accel_ev_ : tempo_ev_;
      span_ = new Audio_span_tempo (last_change_mom_, wpm_);
      span_ramping_ = ramping_;
      announce_element (Audio_element_info (span_, cause));
    }

  // Create Audio_tempo on discrete changes.  Create Audio_tempo at every
  // timestep while ramping.  We may ignore some of these after the fact if
  // they are closer together than they need to be.
  const auto &now = now_mom ();
  if (tempo_ev_ || accel_ev_ || ramping_ || (last_change_mom_ == now))
    {
      close_tempo (now);
      auto *const cause = tempo_ev_ ? tempo_ev_ : span_->cause ();
      spanned_changes_.push_back (new Audio_tempo (span_, now));
      announce_element (Audio_element_info (spanned_changes_.back (), cause));
    }
}

void
Tempo_performer::stop_translation_timestep ()
{
  accel_ev_ = nullptr;
  accel_stop_ev_ = nullptr;
  tempo_ev_ = nullptr;

  accel_stop_wpm_ = -Rational::infinity ();
}

void
Tempo_performer::finalize ()
{
  if (span_)
    {
      if (ramping_ && !spanned_changes_.empty ())
        {
          auto *const last = spanned_changes_.back ();
          if (!last->has_end_moment ())
            {
              last->warning (_ ("unterminated tempo change"));
            }
        }
      close_span_tempo ();
    }
}

void
Tempo_performer::boot ()
{
  // TODO: caesura
  ADD_LISTENER (tempo_change);
  ADD_LISTENER (tempo_gradual_change);
}

ADD_TRANSLATOR (Tempo_performer,
                /* doc */
                R"(

                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
tempoWholesPerMinute
                )",

                /* write */
                R"(

                )");
