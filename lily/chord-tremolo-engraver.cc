/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "misc.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "stem-tremolo.hh"
#include "stem.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::string;

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
  Stream_event *repeat_ = nullptr;

  Spanner *beam_ = nullptr;
  // Store the pointer to the previous stem, so we can create a beam if
  // necessary and end the spanner
  Grob *previous_stem_ = nullptr;

protected:
  void finalize () override;
  void process_music ();
  void listen_tremolo_span (Stream_event *);
  void acknowledge_stem (Grob_info);
};

Chord_tremolo_engraver::Chord_tremolo_engraver (Context *c)
  : Engraver (c)
{
}

void
Chord_tremolo_engraver::listen_tremolo_span (Stream_event *ev)
{
  Direction span_dir
    = from_scm<Direction> (get_property (ev, "span-direction"));
  if (span_dir == START)
    {
      assign_event_once (repeat_, ev);
    }
  else if (span_dir == STOP)
    {
      if (!repeat_)
        ev->warning (_ ("No tremolo to end"));
      repeat_ = nullptr;
      beam_ = nullptr;
      previous_stem_ = nullptr;
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
      repeat_->warning (_ ("unterminated chord tremolo"));
      announce_end_grob (beam_, SCM_EOL);
      beam_->suicide ();
    }
}

void
Chord_tremolo_engraver::acknowledge_stem (Grob_info info)
{
  if (beam_)
    {
      int tremolo_type = from_scm (get_property (repeat_, "tremolo-type"), 1);
      int flags = std::max (0, intlog2 (tremolo_type) - 2);
      int repeat_count = from_scm (get_property (repeat_, "repeat-count"), 1);
      int gap_count = std::min (flags, intlog2 (repeat_count) + 1);

      Grob *s = info.grob ();
      if (previous_stem_)
        {
          // FIXME: We know that the beam has ended only in listen_tremolo_span
          //        but then it is too late for Spanner_break_forbid_engraver
          //        to allow a line break... So, as a nasty hack, announce the
          //        spanner's end after each note except the first. The only
          //        "drawback" is that for multi-note tremolos a break would
          //        theoretically be allowed after the second note (but since
          //        that note is typically not at a barline, I don't think
          //        anyone will ever notice!)
          announce_end_grob (beam_, previous_stem_->self_scm ());
          // Create the whole beam between previous and current note
          Stem::set_beaming (previous_stem_, flags, RIGHT);
          Stem::set_beaming (s, flags, LEFT);
        }

      if (Stem::duration_log (s) != 1)
        set_property (beam_, "gap-count", to_scm (gap_count));

      if (info.ultimate_event_cause ()->in_event_class ("rhythmic-event"))
        Beam::add_stem (beam_, s);
      else
        s->warning (_ ("stem must have Rhythmic structure"));

      // Store current grob, so we can possibly end the spanner here (and
      // reset the beam direction to RIGHT)
      previous_stem_ = s;
    }
}

void
Chord_tremolo_engraver::boot ()
{
  ADD_LISTENER (tremolo_span);
  ADD_ACKNOWLEDGER (stem);
}

ADD_TRANSLATOR (Chord_tremolo_engraver,
                /* doc */
                R"(
Generate beams for tremolo repeats.
                )",

                /* create */
                R"(
Beam
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
