/*
  tab-note-heads-engraver.cc -- part of GNU LilyPond

  based on note-heads-engraver.cc, by Jean-Baptiste Lamy <jiba@tuxfamily.org>,

  (c) 2002--2006
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
      SCM stringTunings = get_property ("stringTunings");
      int number_of_strings = scm_ilength (stringTunings);
      bool high_string_one = to_boolean (get_property ("highStringOne"));

      Stream_event *event = note_events_[i];
      Item *note = make_item ("TabNoteHead", event->self_scm ());

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

      int tab_string;
      bool string_found;
      if (tabstring_event)
	{
	  tab_string = scm_to_int (tabstring_event->get_property ("string-number"));
	  string_found = true;
	}
      else
	{
	  tab_string = high_string_one ? 1 : number_of_strings;
	  string_found = false;
	}

      Duration dur = *unsmob_duration (event->get_property ("duration"));

      SCM scm_pitch = event->get_property ("pitch");
      SCM proc = get_property ("tablatureFormat");
      SCM min_fret_scm = get_property ("minimumFret");
      int min_fret = scm_is_number (min_fret_scm) ? scm_to_int (min_fret_scm) : 0;

      while (!string_found)
	{
	  int fret = unsmob_pitch (scm_pitch)->semitone_pitch ()
	    - scm_to_int (scm_list_ref (stringTunings, scm_from_int (tab_string - 1)));
	  if (fret < min_fret)
	    tab_string += high_string_one ? 1 : -1;
	  else
	    string_found = true;
	}

      SCM text = scm_call_3 (proc, scm_from_int (tab_string), stringTunings, scm_pitch);

      int pos = 2 * tab_string - number_of_strings - 1; // No tab-note between the string !!!
      if (to_boolean (get_property ("stringOneTopmost")))
	pos = -pos;

      note->set_property ("text", text);

      note->set_property ("staff-position", scm_from_int (pos));
      notes_.push_back (note);
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
		/* doc */ "Generate one or more tablature noteheads from event of type NoteEvent.",
		/* create */
		"TabNoteHead "
		,

		/* read */
		"middleCPosition "
		"stringTunings "
		"minimumFret "
		"tablatureFormat "
		"highStringOne "
		"stringOneTopmost ",

		/* write */ "");

