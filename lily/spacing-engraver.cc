/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "moment.hh"
#include "note-spacing.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "pqueue.hh"
#include "spanner.hh"
#include "staff-spacing.hh"
#include "stream-event.hh"

#include "translator.icc"

using std::vector;

struct Rhythmic_tuple
{
  Grob_info info_;
  Moment end_;

  Rhythmic_tuple (Grob_info i, Moment m)
    : info_ (i),
      end_ (m)
  {
  }

  static int time_compare (Rhythmic_tuple const &, Rhythmic_tuple const &);
};

inline int
compare (Rhythmic_tuple const &a, Rhythmic_tuple const &b)
{
  return Rhythmic_tuple::time_compare (a, b);
}

int
Rhythmic_tuple::time_compare (Rhythmic_tuple const &h1,
                              Rhythmic_tuple const &h2)
{
  return (h1.end_ - h2.end_).main_part_.sign ();
}

/****************************************************************/

/*
  Acknowledge rhythmic elements, for initializing spacing fields in
  the columns.
*/
class Spacing_engraver : public Engraver
{
  PQueue<Rhythmic_tuple> playing_durations_;
  vector<Rhythmic_tuple> now_durations_;
  vector<Rhythmic_tuple> stopped_durations_;
  Moment now_;
  Spanner *spacing_;
  Stream_event *start_section_;

  TRANSLATOR_DECLARATIONS (Spacing_engraver);

protected:
  void acknowledge_staff_spacing (Grob_info);
  void acknowledge_note_spacing (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_rhythmic_grob (Grob_info);
  void listen_spacing_section (Stream_event *);

  void start_translation_timestep ();
  void stop_translation_timestep ();
  void process_music ();
  void add_starter_duration (Grob_info i);

  void initialize () override;
  void finalize () override;

  void start_spanner ();
  void stop_spanner ();
};

Spacing_engraver::Spacing_engraver (Context *c)
  : Engraver (c)
{
  spacing_ = 0;
  start_section_ = 0;
}

void
Spacing_engraver::listen_spacing_section (Stream_event *ev)
{
  assign_event_once (start_section_, ev);
}

void
Spacing_engraver::process_music ()
{
  if (start_section_ && spacing_)
    stop_spanner ();

  if (!spacing_)
    start_spanner ();
}

void
Spacing_engraver::start_spanner ()
{
  assert (!spacing_);

  spacing_ = make_spanner ("SpacingSpanner", SCM_EOL);
  auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
  spacing_->set_bound (LEFT, col);
}

void
Spacing_engraver::initialize ()
{
  now_ = now_mom ();
}

void
Spacing_engraver::finalize ()
{
  stop_spanner ();
}

void
Spacing_engraver::stop_spanner ()
{
  if (spacing_)
    {
      Grob *p = unsmob<Grob> (get_property (this, "currentCommandColumn"));

      spacing_->set_bound (RIGHT, p);
      spacing_ = 0;
    }
}

void
Spacing_engraver::acknowledge_note_spacing (Grob_info i)
{
  Pointer_group_interface::add_grob (spacing_, ly_symbol2scm ("wishes"),
                                     i.grob ());
}

void
Spacing_engraver::acknowledge_staff_spacing (Grob_info i)
{
  Pointer_group_interface::add_grob (spacing_, ly_symbol2scm ("wishes"),
                                     i.grob ());
}

void
Spacing_engraver::acknowledge_rhythmic_grob (Grob_info i)
{
  add_starter_duration (i);
}

void
Spacing_engraver::acknowledge_rhythmic_head (Grob_info i)
{
  add_starter_duration (i);
}

void
Spacing_engraver::add_starter_duration (Grob_info i)
{
  if (i.grob ()->internal_has_interface (
        ly_symbol2scm ("lyric-syllable-interface"))
      || i.grob ()->internal_has_interface (
        ly_symbol2scm ("multi-measure-interface")))
    return;

  /*
    only pay attention to durations that are not grace notes.
  */
  if (!now_.grace_part_)
    {
      Stream_event *r = i.event_cause ();
      if (r && r->in_event_class ("rhythmic-event"))
        {
          auto len = get_event_length (r, now_);
          Rhythmic_tuple t (i, now_mom () + len);
          now_durations_.push_back (t);
        }
    }
}

void
Spacing_engraver::stop_translation_timestep ()
{
  Paper_column *musical_column
    = unsmob<Paper_column> (get_property (this, "currentMusicalColumn"));

  if (!spacing_)
    start_spanner ();

  set_object (musical_column, "spacing", spacing_->self_scm ());
  set_object (unsmob<Grob> (get_property (this, "currentCommandColumn")),
              "spacing", spacing_->self_scm ());

  SCM proportional = get_property (this, "proportionalNotationDuration");
  if (unsmob<Moment> (proportional))
    {
      set_property (musical_column, "shortest-playing-duration", proportional);
      set_property (musical_column, "shortest-starter-duration", proportional);
      set_property (musical_column, "used", SCM_BOOL_T);
      return;
    }

  auto shortest_playing = Moment::infinity ();
  for (vsize i = 0; i < playing_durations_.size (); i++)
    {
      Stream_event *ev = playing_durations_[i].info_.event_cause ();
      if (ev)
        {
          auto m = get_event_length (ev);
          shortest_playing = std::min (shortest_playing, m);
        }
    }
  auto starter = Moment::infinity ();

  for (vsize i = 0; i < now_durations_.size (); i++)
    {
      auto m = get_event_length (now_durations_[i].info_.event_cause ());
      if (m)
        {
          starter = std::min (starter, m);
          playing_durations_.insert (now_durations_[i]);
        }
    }
  now_durations_.clear ();

  shortest_playing = std::min (shortest_playing, starter);

  assert (starter);
  SCM sh = shortest_playing.smobbed_copy ();
  SCM st = starter.smobbed_copy ();

  set_property (musical_column, "shortest-playing-duration", sh);
  set_property (musical_column, "shortest-starter-duration", st);
}

void
Spacing_engraver::start_translation_timestep ()
{
  start_section_ = 0;

  now_ = now_mom ();
  stopped_durations_.clear ();

  while (playing_durations_.size () && playing_durations_.front ().end_ < now_)
    playing_durations_.delmin ();
  while (playing_durations_.size () && playing_durations_.front ().end_ == now_)
    stopped_durations_.push_back (playing_durations_.get ());
}

void
Spacing_engraver::boot ()
{
  ADD_LISTENER (spacing_section);
  ADD_ACKNOWLEDGER (staff_spacing);
  ADD_ACKNOWLEDGER (note_spacing);
  ADD_ACKNOWLEDGER (rhythmic_head);
  ADD_ACKNOWLEDGER (rhythmic_grob);
}

ADD_TRANSLATOR (Spacing_engraver,
                /* doc */
                R"(
Make a @code{SpacingSpanner} and do bookkeeping of shortest starting and
playing notes.
                )",

                /* create */
                R"(
SpacingSpanner
                )",

                /* read */
                R"(
currentMusicalColumn
currentCommandColumn
proportionalNotationDuration
                )",

                /* write */
                R"(

                )");
