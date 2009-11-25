/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2009 Han-Wen Nienhuys

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

#include <cctype>
#include <cstdio>
using namespace std;

#include "context.hh"
#include "item.hh"
#include "engraver.hh"
#include "pitch.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/**
   make (guitar-like) tablature note
*/
class Fretboard_engraver : public Engraver
{
  Item *fret_board_;
  
  vector<Stream_event*> note_events_;
  vector<Stream_event*> tabstring_events_;
public:
  TRANSLATOR_DECLARATIONS (Fretboard_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();
  virtual void derived_mark() const;
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (string_number);

private:
  SCM last_fret_notes_;
};


void
Fretboard_engraver::derived_mark () const
{
  scm_gc_mark (last_fret_notes_);
}

Fretboard_engraver::Fretboard_engraver ()
{
  fret_board_ = 0;
  last_fret_notes_ = SCM_EOL;
}

IMPLEMENT_TRANSLATOR_LISTENER (Fretboard_engraver, note);
void
Fretboard_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Fretboard_engraver, string_number);
void
Fretboard_engraver::listen_string_number (Stream_event *ev)
{
  tabstring_events_.push_back (ev);
}

void
Fretboard_engraver::process_music ()
{
  if (!note_events_.size ())
    return ;

  fret_board_ = make_item ("FretBoard", note_events_[0]->self_scm ());
  SCM fret_notes = ly_cxx_vector_to_list (note_events_);
  SCM proc = get_property ("noteToFretFunction");
  if (ly_is_procedure (proc))
    {
     scm_call_4 (proc,
		 context ()->self_scm (),
		 fret_board_->self_scm (),
		 fret_notes,	       
		 ly_cxx_vector_to_list (tabstring_events_));
    }
  SCM changes = get_property("chordChanges");
  if (to_boolean (changes) && scm_is_pair(last_fret_notes_)
      && ly_is_equal (last_fret_notes_, fret_notes))
    fret_board_->set_property ("begin-of-line-visible", SCM_BOOL_T);
  
  last_fret_notes_ = fret_notes;
}

void
Fretboard_engraver::stop_translation_timestep ()
{
  fret_board_ = 0;
  note_events_.clear ();
  tabstring_events_.clear ();
}

ADD_TRANSLATOR (Fretboard_engraver,
		/* doc */
		"Generate one or more tablature noteheads from event of type"
		" @code{NoteEvent}.",

		/* create */
		"FretBoard ",

		/* read */
                "chordChanges "
		"stringTunings "
		"minimumFret "
                "maximumFretStretch "
		"tablatureFormat "
		"highStringOne "
                "predefinedDiagramTable",

		/* write */
		""
		);

