/*
  note-name-engraver.cc -- implement Note_name_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1999--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "engraver.hh"
#include "item.hh"

class Note_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Note_name_engraver);

  vector<Music*> events_;
  vector<Item*> texts_;
  virtual bool try_music (Music *m);
  void process_music ();
  void stop_translation_timestep ();
};

bool
Note_name_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("note-event"))
    {
      events_.push_back (m);
      return true;
    }
  return false;
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
      t->set_property ("text", scm_makfrom0str (s.c_str ()));
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

#include "translator.icc"

ADD_TRANSLATOR (Note_name_engraver,
		/* doc */ "",
		/* create */ "NoteName",
		/* accept */ "note-event",
		/* read */ "printOctaveNames",
		/* write */ "");
