/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "audio-column.hh"
#include "audio-item.hh"
#include "global-context.hh"
#include "performer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

using std::vector;

class Note_performer : public Performer
{
public:
  TRANSLATOR_DECLARATIONS (Note_performer);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void listen_note (Stream_event *);
  void listen_breathing (Stream_event *);
  void listen_tie (Stream_event *);
  void listen_articulation (Stream_event *);

private:
  vector<Stream_event *> note_evs_, script_evs_;
  vector<Audio_note *> notes_;

  vector<Audio_note *> last_notes_;
  Moment last_start_;
};

void
Note_performer::process_music ()
{
  if (!note_evs_.size ())
    return;

  Pitch transposing;
  SCM prop = get_property ("instrumentTransposition");
  if (unsmob<Pitch> (prop))
    transposing = *unsmob<Pitch> (prop);

  for (vsize i = 0; i < note_evs_.size (); i++)
    {
      Stream_event *n = note_evs_[i];
      SCM pit = n->get_property ("pitch");

      if (Pitch *pitp = unsmob<Pitch> (pit))
        {
          SCM articulations = n->get_property ("articulations");
          Stream_event *tie_event = 0;
          Moment len = get_event_length (n, now_mom ());
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
              SCM f = ev->get_property ("midi-length");
              if (ly_is_procedure (f))
                len = robust_scm2moment (scm_call_2 (f, len.smobbed_copy (),
                                                     context ()->self_scm ()),
                                         len);
              velocity += robust_scm2int (
                  ev->get_property ("midi-extra-velocity"), 0);
            }

          Audio_note *p
              = new Audio_note (*pitp, len, tie_event, transposing, velocity);
          Audio_element_info info (p, n);
          announce_element (info);
          notes_.push_back (p);

          /*
            Grace notes shorten the previous non-grace note. If it was
            part of a tie, shorten the first note in the tie.
           */
          if (now_mom ().grace_part_)
            {
              if (last_start_.grace_part_ == Rational (0))
                {
                  for (vsize i = 0; i < last_notes_.size (); i++)
                    {
                      Audio_note *tie_head = last_notes_[i]->tie_head ();
                      Moment start = tie_head->audio_column_->when ();
                      // Shorten the note if it would overlap. It might
                      // not if there's a rest in between.
                      if (start + tie_head->length_mom_ > now_mom ())
                        tie_head->length_mom_ = now_mom () - start;
                    }
                }
            }
        }
    }
}

void
Note_performer::stop_translation_timestep ()
{
  if (note_evs_.size ())
    {
      last_notes_ = notes_;
      last_start_ = now_mom ();
    }

  notes_.clear ();
  note_evs_.clear ();
  script_evs_.clear ();
}

void
Note_performer::listen_note (Stream_event *ev)
{
  note_evs_.push_back (ev);
}

void
Note_performer::listen_tie (Stream_event *ev)
{
  script_evs_.push_back (ev);
}

void
Note_performer::listen_articulation (Stream_event *ev)
{
  script_evs_.push_back (ev);
}

void
Note_performer::listen_breathing (Stream_event *ev)
{
  // Shorten previous note if needed
  SCM f = ev->get_property ("midi-length");
  if (ly_is_procedure (f))
    for (vsize i = 0; i < last_notes_.size (); i++)
      {
        // Pass midi-length the available time since the last note started,
        // including any intervening rests. It returns how much is left for the
        // note.
        Moment start = last_notes_[i]->audio_column_->when ();
        Moment available = now_mom () - start;
        Moment len = robust_scm2moment (
            scm_call_2 (f, available.smobbed_copy (), context ()->self_scm ()),
            available);
        // Take time from the first note of the tie, since it has all the
        // length.
        Audio_note *tie_head = last_notes_[i]->tie_head ();
        len += start - tie_head->audio_column_->when ();
        if (len < tie_head->length_mom_)
          tie_head->length_mom_ = len;
      }
}

void
Note_performer::boot ()
{
  ADD_LISTENER (Note_performer, note);
  ADD_LISTENER (Note_performer, breathing);
  ADD_LISTENER (Note_performer, tie);
  ADD_LISTENER (Note_performer, articulation);
}

ADD_TRANSLATOR (Note_performer,
                /* doc */
                "",

                /* create */
                "",

                /* read */
                "",

                /* write */
                "");

Note_performer::Note_performer (Context *c) : Performer (c) {}
