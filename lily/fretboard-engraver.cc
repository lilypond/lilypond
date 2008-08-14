/*
  fretboard-engraver.cc -- part of GNU LilyPond

  (c)  2006  Han-Wen Nienhuys
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
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (string_number);
  void process_music ();

  void stop_translation_timestep ();
};

Fretboard_engraver::Fretboard_engraver ()
{
  fret_board_ = 0;
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

  SCM proc = get_property ("noteToFretFunction");
  if (ly_is_procedure (proc))
    {
      scm_call_4 (proc,
		  context ()->self_scm (),
		  fret_board_->self_scm (),
			       
		  ly_cxx_vector_to_list (note_events_),
		  ly_cxx_vector_to_list (tabstring_events_));
    }
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
		"stringTunings "
		"minimumFret "
                "maximumFretStretch "
		"tablatureFormat "
		"highStringOne "
                "predefinedDiagramTable",

		/* write */
		""
		);

