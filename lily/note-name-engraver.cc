/*
  note-name-engraver.cc -- implement Note_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "item.hh"
#include "pitch.hh"
#include "stream-event.hh"

#include "translator.icc"

class Note_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Note_name_engraver);

  vector<Stream_event*> events_;
  vector<Item*> texts_;
  DECLARE_TRANSLATOR_LISTENER (note);
  void process_music ();
  void stop_translation_timestep ();
};

IMPLEMENT_TRANSLATOR_LISTENER (Note_name_engraver, note);
void
Note_name_engraver::listen_note (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Note_name_engraver::process_music ()
{
  string s;
  for (vsize i = 0; i < events_.size (); i++)
    {
      if (i)
	s += " ";
      Pitch p = *unsmob_pitch (events_[i]->get_property ("pitch"));

      if (!to_boolean (get_property ("printOctaveNames")))
	p = Pitch (-1, p.get_notename (), p.get_alteration ());

      s += p.to_string ();
    }
  if (s.length ())
    {
      Item *t = make_item ("NoteName", events_[0]->self_scm ());
      t->set_property ("text", ly_string2scm (s));
      texts_.push_back (t);
    }
}

void
Note_name_engraver::stop_translation_timestep ()
{
  texts_.clear ();
  events_.clear ();
}

Note_name_engraver::Note_name_engraver ()
{
}

ADD_TRANSLATOR (Note_name_engraver,
		/* doc */
		"Print pitches as words.",

		/* create */
		"NoteName ",

		/* read */
		"printOctaveNames ",

		/* write */
		""
		);
