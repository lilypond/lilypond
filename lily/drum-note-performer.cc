/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Jan Nieuwenhuizen <janneke@gnu.org>

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
#include "audio-item.hh"
#include "audio-column.hh"
#include "global-context.hh"
#include "stream-event.hh"
#include "translator.icc"
#include "warn.hh"

using std::vector;

class Drum_note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Drum_note_performer);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void listen_note (Stream_event *);
  void listen_tie (Stream_event *);
  void listen_articulation (Stream_event *);

private:
  vector<Stream_event *> note_evs_, script_evs_;
};

Drum_note_performer::Drum_note_performer (Context *c)
  : Performer (c)
{
}

void
Drum_note_performer::process_music ()
{
  SCM tab = get_property (this, "drumPitchTable");

  while (note_evs_.size ())
    {
      Stream_event *n = note_evs_.back ();
      note_evs_.pop_back ();
      SCM sym = get_property (n, "drum-type");
      SCM defn = SCM_EOL;

      if (scm_is_symbol (sym) && from_scm<bool> (scm_hash_table_p (tab)))
        defn = scm_hashq_ref (tab, sym, SCM_EOL);

      if (Pitch *pit = unsmob<Pitch> (defn))
        {
          SCM articulations = get_property (n, "articulations");
          Stream_event *tie_event = 0;
          auto len = get_event_length (n, now_mom ());
          int velocity = 0;

          for (vsize j = script_evs_.size (); j--;)
            articulations
              = scm_cons (script_evs_[j]->self_scm (), articulations);

          for (SCM s = articulations; scm_is_pair (s); s = scm_cdr (s))
            {
              Stream_event *ev = unsmob<Stream_event> (scm_car (s));
              if (!ev)
                continue;

              if (ev->in_event_class ("tie-event"))
                tie_event = ev;
              SCM f = get_property (ev, "midi-length");
              if (ly_is_procedure (f))
                {
                  len = from_scm (
                    ly_call (f, len.smobbed_copy (), context ()->self_scm ()),
                    len);
                }
              velocity
                += from_scm (get_property (ev, "midi-extra-velocity"), 0);
            }

          announce<Audio_note> (n, *pit, len, tie_event, Pitch (0, 0),
                                velocity);
        }
    }

  note_evs_.clear ();
}

void
Drum_note_performer::stop_translation_timestep ()
{
  note_evs_.clear ();
  script_evs_.clear ();
}

void
Drum_note_performer::listen_note (Stream_event *ev)
{
  note_evs_.push_back (ev);
}

void
Drum_note_performer::listen_tie (Stream_event *ev)
{
  script_evs_.push_back (ev);
}

void
Drum_note_performer::listen_articulation (Stream_event *ev)
{
  script_evs_.push_back (ev);
}

void
Drum_note_performer::boot ()
{
  ADD_LISTENER (note);
  ADD_LISTENER (tie);
  ADD_LISTENER (articulation);
}

ADD_TRANSLATOR (Drum_note_performer,
                /* doc */
                R"(
Play drum notes.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
