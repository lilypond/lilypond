/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys, Jean-Baptiste Lamy <jiba@tuxfamily.org>,

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

#include "articulations.hh"
#include "duration.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "context.hh"

#include "translator.icc"

#include <cctype>
#include <cstdio>

using std::vector;

/**
   make (guitar-like) tablature note
*/
class Tab_note_heads_engraver : public Engraver
{
  vector<Stream_event *> note_events_;
  vector<Stream_event *> tabstring_events_;
  vector<Stream_event *> fingering_events_;

public:
  TRANSLATOR_DECLARATIONS (Tab_note_heads_engraver);

protected:
  void listen_note (Stream_event *);
  void listen_string_number (Stream_event *);
  void listen_fingering (Stream_event *);
  void process_music ();

  void stop_translation_timestep ();
};

Tab_note_heads_engraver::Tab_note_heads_engraver (Context *c)
  : Engraver (c)
{
}

void
Tab_note_heads_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
}

void
Tab_note_heads_engraver::listen_string_number (Stream_event *ev)
{
  tabstring_events_.push_back (ev);
}

void
Tab_note_heads_engraver::listen_fingering (Stream_event *ev)
{
  fingering_events_.push_back (ev);
}

void
Tab_note_heads_engraver::process_music ()
{
  SCM tab_strings = articulation_list (note_events_, tabstring_events_,
                                       "string-number-event");
  SCM defined_fingers
    = articulation_list (note_events_, fingering_events_, "fingering-event");
  SCM tab_notes = to_scm_list (note_events_);
  SCM proc = get_property (this, "noteToFretFunction");
  SCM string_fret_finger = SCM_EOL;
  if (ly_is_procedure (proc))
    string_fret_finger = ly_call (proc, context ()->self_scm (), tab_notes,
                                  ly_list (tab_strings, defined_fingers));
  SCM note_entry = SCM_EOL;
  SCM string_number = SCM_EOL;
  SCM fret = SCM_EOL;
  SCM fret_label = SCM_EOL;
  SCM fret_procedure = get_property (this, "tablatureFormat");
  SCM staff_line_procedure = get_property (this, "tabStaffLineLayoutFunction");
  SCM staff_position = SCM_EOL;
  const auto fret_count = static_cast<vsize> (scm_ilength (string_fret_finger));
  bool length_changed = (note_events_.size () != fret_count);
  vsize index;

  if (!scm_is_null (string_fret_finger))
    for (vsize i = 0; i < fret_count; i++)
      {
        note_entry = scm_list_ref (string_fret_finger, to_scm (i));
        string_number = scm_car (note_entry);
        if (scm_is_true (string_number))
          {
            fret = scm_cadr (note_entry);
            fret_label = ly_call (fret_procedure, context ()->self_scm (),
                                  string_number, fret);
            index = length_changed ? 0 : i;
            Item *note
              = make_item ("TabNoteHead", note_events_[index]->self_scm ());
            set_property (note, "text", fret_label);
            staff_position = ly_call (staff_line_procedure,
                                      context ()->self_scm (), string_number);
            set_property (note, "staff-position", staff_position);
          }
      }
}

void
Tab_note_heads_engraver::stop_translation_timestep ()
{
  note_events_.clear ();
  tabstring_events_.clear ();
  fingering_events_.clear ();
}

void
Tab_note_heads_engraver::boot ()
{
  ADD_LISTENER (note);
  ADD_LISTENER (string_number);
  ADD_LISTENER (fingering);
}

ADD_TRANSLATOR (Tab_note_heads_engraver,
                /* doc */
                R"(
Generate one or more tablature note heads from event of type @code{NoteEvent}.
                )",

                /* create */
                R"(
TabNoteHead
                )",

                /* read */
                R"(
defaultStrings
fretLabels
highStringOne
maximumFretStretch
middleCPosition
minimumFret
noteToFretFunction
stringOneTopmost
stringTunings
tablatureFormat
tabStaffLineLayoutFunction
                )",

                /* write */
                R"(

                )");
