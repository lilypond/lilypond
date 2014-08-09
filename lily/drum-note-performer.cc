/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2014 Jan Nieuwenhuizen <janneke@gnu.org>

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

class Drum_note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Drum_note_performer);

protected:
  void stop_translation_timestep ();
  void process_music ();
  DECLARE_TRANSLATOR_LISTENER (note);
private:
  vector<Stream_event *> note_evs_;
};

Drum_note_performer::Drum_note_performer ()
{
}

void
Drum_note_performer::process_music ()
{
  SCM tab = get_property ("drumPitchTable");

  while (note_evs_.size ())
    {
      Stream_event *n = note_evs_.back ();
      note_evs_.pop_back ();
      SCM sym = n->get_property ("drum-type");
      SCM defn = SCM_EOL;

      if (scm_is_symbol (sym)
          && (scm_hash_table_p (tab) == SCM_BOOL_T))
        defn = scm_hashq_ref (tab, sym, SCM_EOL);

      if (Pitch *pit = Pitch::unsmob (defn))
        {
          SCM articulations = n->get_property ("articulations");
          Stream_event *tie_event = 0;
          Moment len = get_event_length (n, now_mom ());
          int velocity = 0;
          for (SCM s = articulations; scm_is_pair (s); s = scm_cdr (s))
            {
              Stream_event *ev = Stream_event::unsmob (scm_car (s));
              if (!ev)
                continue;

              if (ev->in_event_class ("tie-event"))
                tie_event = ev;
              SCM f = ev->get_property ("midi-length");
              if (ly_is_procedure (f))
                len = robust_scm2moment (scm_call_2 (f, len.smobbed_copy (),
                                                     context ()->self_scm ()),
                                         len);
              velocity += robust_scm2int (ev->get_property ("midi-extra-velocity"), 0);
            }

          Audio_note *p = new Audio_note (*pit, len,
                                          tie_event, Pitch (0, 0), velocity);
          Audio_element_info info (p, n);
          announce_element (info);
        }
    }

  note_evs_.clear ();
}

void
Drum_note_performer::stop_translation_timestep ()
{
  note_evs_.clear ();
}

IMPLEMENT_TRANSLATOR_LISTENER (Drum_note_performer, note);
void
Drum_note_performer::listen_note (Stream_event *ev)
{
  note_evs_.push_back (ev);
}

ADD_TRANSLATOR (Drum_note_performer,
                /* doc */
                "Play drum notes.",

                /* create */
                "",

                /* read */
                "",

                /* write */
                ""
               );
