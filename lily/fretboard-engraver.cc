/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys

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

#include "articulations.hh"
#include "context.hh"
#include "item.hh"
#include "engraver.hh"
#include "pitch.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

#include <cctype>
#include <cstdio>

using std::vector;

/**
   make (guitar-like) tablature note
*/
class Fretboard_engraver : public Engraver
{
  Item *fret_board_;

  vector<Stream_event *> note_events_;
  vector<Stream_event *> tabstring_events_;
  vector<Stream_event *> fingering_events_;

public:
  TRANSLATOR_DECLARATIONS (Fretboard_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();
  void derived_mark () const override;
  void listen_note (Stream_event *);
  void listen_string_number (Stream_event *);
  void listen_fingering (Stream_event *);

private:
  SCM last_placements_;
};

void
Fretboard_engraver::derived_mark () const
{
  scm_gc_mark (last_placements_);
}

Fretboard_engraver::Fretboard_engraver (Context *c)
  : Engraver (c)
{
  fret_board_ = 0;
  last_placements_ = SCM_BOOL_F;
}

void
Fretboard_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
}

void
Fretboard_engraver::listen_string_number (Stream_event *ev)
{
  tabstring_events_.push_back (ev);
}

void
Fretboard_engraver::listen_fingering (Stream_event *ev)
{
  fingering_events_.push_back (ev);
}

void
Fretboard_engraver::process_music ()
{
  if (!note_events_.size ())
    return;

  SCM tab_strings = articulation_list (note_events_, tabstring_events_,
                                       "string-number-event");
  SCM fingers
    = articulation_list (note_events_, fingering_events_, "fingering-event");
  fret_board_ = make_item ("FretBoard", note_events_[0]->self_scm ());
  SCM fret_notes = to_scm_list (note_events_);
  SCM proc = get_property (this, "noteToFretFunction");
  if (ly_is_procedure (proc))
    ly_call (proc, context ()->self_scm (), fret_notes,
             ly_list (tab_strings, fingers), fret_board_->self_scm ());
  SCM changes = get_property (this, "chordChanges");
  SCM placements = get_property (fret_board_, "dot-placement-list");
  if (from_scm<bool> (changes) && ly_is_equal (last_placements_, placements))
    set_property (fret_board_, "begin-of-line-visible", SCM_BOOL_T);

  last_placements_ = placements;
}

void
Fretboard_engraver::stop_translation_timestep ()
{
  fret_board_ = 0;
  note_events_.clear ();
  tabstring_events_.clear ();
  fingering_events_.clear ();
}

void
Fretboard_engraver::boot ()
{
  ADD_LISTENER (note);
  ADD_LISTENER (string_number);
  ADD_LISTENER (fingering);
}

ADD_TRANSLATOR (Fretboard_engraver,
                /* doc */
                R"(
Generate fret diagram from one or more events of type @code{NoteEvent}.
                )",

                /* create */
                R"(
FretBoard
                )",

                /* read */
                R"(
chordChanges
defaultStrings
highStringOne
maximumFretStretch
minimumFret
noteToFretFunction
predefinedDiagramTable
stringTunings
tablatureFormat
                )",

                /* write */
                R"(

                )");
