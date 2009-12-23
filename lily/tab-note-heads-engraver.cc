/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2009 Han-Wen Nienhuys, Jean-Baptiste Lamy <jiba@tuxfamily.org>,

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

#include "engraver.hh"

using namespace std;

#include "duration.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "context.hh"

#include "translator.icc"

/**
   make (guitar-like) tablature note
*/
class Tab_note_heads_engraver : public Engraver
{
  vector<Item*> notes_;

  vector<Stream_event*> note_events_;
  vector<Stream_event*> tabstring_events_;
public:
  TRANSLATOR_DECLARATIONS (Tab_note_heads_engraver);

protected:
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (string_number);
  void process_music ();

  void stop_translation_timestep ();
};

Tab_note_heads_engraver::Tab_note_heads_engraver ()
{
}

IMPLEMENT_TRANSLATOR_LISTENER (Tab_note_heads_engraver, note);
void
Tab_note_heads_engraver::listen_note (Stream_event *ev)
{
  note_events_.push_back (ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Tab_note_heads_engraver, string_number);
void
Tab_note_heads_engraver::listen_string_number (Stream_event *ev)
{
  tabstring_events_.push_back (ev);
}

void
Tab_note_heads_engraver::process_music ()
{
  vsize j = 0;
  for (vsize i = 0; i < note_events_.size (); i++)
    {
      SCM string_tunings = get_property ("stringTunings");
      int string_count = scm_ilength (string_tunings);
      bool high_string_one = to_boolean (get_property ("highStringOne"));

      Stream_event *event = note_events_[i];

      Stream_event *tabstring_event = 0;

      for (SCM s = event->get_property ("articulations");
	   !tabstring_event && scm_is_pair (s); s = scm_cdr (s))
	{
	  Stream_event *art = unsmob_stream_event (scm_car (s));

	  if (art->in_event_class ("string-number-event"))
	    tabstring_event = art;
	}

      if (!tabstring_event && j < tabstring_events_.size ())
	{
	  tabstring_event = tabstring_events_[j];
	  if (j + 1 < tabstring_events_.size ())
	    j++;
	}

      int string_number = 0;
      if (tabstring_event)
	string_number = scm_to_int (tabstring_event->get_property ("string-number"));

      if (!string_number)
	{
	  SCM scm_pitch = event->get_property ("pitch");
	  int min_fret = robust_scm2int (get_property ("minimumFret"), 0);
	  int start = (high_string_one) ? 1 : string_count;
	  int end = (high_string_one) ? string_count+1 : 0;

	  int i = start;
	  do
	    {
	      int fret = unsmob_pitch (scm_pitch)->rounded_semitone_pitch ()
		- scm_to_int (robust_list_ref (i - 1, string_tunings));
	  
	      if (fret >= min_fret)
		{
		  string_number = i;
		  break;
		}
	      i += high_string_one ? 1 : -1;
	    }
	  while (i != end);
	}
      
      if (string_number)
	{
	  SCM proc = get_property ("tablatureFormat");
	  SCM text = scm_call_3 (proc, scm_from_int (string_number),
				 context ()->self_scm (),
				 event->self_scm ());
	  Item *note = make_item ("TabNoteHead", event->self_scm ());
	  note->set_property ("text", text);


	  int pos = 2 * string_number - string_count - 1; // No tab-note between the string !!!
	  if (to_boolean (get_property ("stringOneTopmost")))
	    pos = - pos;

	  note->set_property ("staff-position", scm_from_int (pos));
      
	  notes_.push_back (note);
	}
      else
	event->origin ()->warning ("could not calculate a string number.");
    }
}
void
Tab_note_heads_engraver::stop_translation_timestep ()
{
  notes_.clear ();
  note_events_.clear ();
  tabstring_events_.clear ();
}

ADD_TRANSLATOR (Tab_note_heads_engraver,
		/* doc */
		"Generate one or more tablature noteheads from event of type"
		" @code{NoteEvent}.",

		/* create */
		"TabNoteHead ",

		/* read */
		"fretLabels "
		"highStringOne "
		"middleCPosition "
		"minimumFret "
		"stringOneTopmost "
		"stringTunings "
		"tablatureFormat ",

		/* write */ ""
		);

