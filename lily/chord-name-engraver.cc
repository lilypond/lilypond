/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "chord-name.hh"
#include "context.hh"
#include "dimensions.hh"
#include "engraver.hh"
#include "font-interface.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "protected-scm.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

class Chord_name_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Chord_name_engraver);
protected:
  void stop_translation_timestep ();
  void process_music ();
  virtual void finalize ();
  virtual void derived_mark () const;
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (rest);
private:
  Item *chord_name_;
  vector<Stream_event*> notes_;
 
  SCM last_chord_;
  Stream_event *rest_event_;
};

void
Chord_name_engraver::finalize ()
{
}

void
Chord_name_engraver::derived_mark () const
{
  scm_gc_mark (last_chord_);
}

Chord_name_engraver::Chord_name_engraver ()
{
  chord_name_ = 0;
  last_chord_ = SCM_EOL;
  rest_event_ = 0;
}

void
Chord_name_engraver::process_music ()
{ 
  SCM markup;
  SCM bass = SCM_EOL;
  SCM inversion = SCM_EOL;
  SCM pitches = SCM_EOL;

  if (rest_event_)
    {
      SCM no_chord_markup = get_property ("noChordSymbol");
      if (!Text_interface::is_markup (no_chord_markup))
        return;
      markup = no_chord_markup;
    }
  else
    {  
      if (!notes_.size ())
        return;

      Stream_event *inversion_event = 0;
      for (vsize i = 0; i < notes_.size (); i++)
      {
        Stream_event *n = notes_[i];
        SCM p = n->get_property ("pitch");
        if (!unsmob_pitch (p))
          continue;

        if (n->get_property ("inversion") == SCM_BOOL_T)
        {
          inversion_event = n;
          inversion = p;
        }
        else if (n->get_property ("bass") == SCM_BOOL_T)
          bass = p;
        else
          pitches = scm_cons (p, pitches);
      }

      if (inversion_event)
      {
        SCM oct = inversion_event->get_property ("octavation");
        if (scm_is_number (oct))
        {
          Pitch *p = unsmob_pitch (inversion_event->get_property ("pitch"));
          int octavation = scm_to_int (oct);
          Pitch orig = p->transposed (Pitch (-octavation, 0, 0));

          pitches = scm_cons (orig.smobbed_copy (), pitches);
        }
        else
          programming_error ("inversion does not have original pitch");
      }

      pitches = scm_sort_list (pitches, Pitch::less_p_proc);

      SCM name_proc = get_property ("chordNameFunction");
      markup = scm_call_4 (name_proc, pitches, bass, inversion,
          context ()->self_scm ());
    }
  /*
    Ugh.
  */
  SCM chord_as_scm = scm_cons (pitches, scm_cons (bass, inversion));

  chord_name_ = make_item ("ChordName", 
      rest_event_ ? rest_event_->self_scm () : notes_[0]->self_scm ());
  chord_name_->set_property ("text", markup);

  SCM chord_changes = get_property("chordChanges");
  if (to_boolean (chord_changes) && scm_is_pair (last_chord_)
      && ly_is_equal (chord_as_scm, last_chord_))
    chord_name_->set_property ("begin-of-line-visible", SCM_BOOL_T);

  last_chord_ = chord_as_scm;
}

IMPLEMENT_TRANSLATOR_LISTENER (Chord_name_engraver, note);
void
Chord_name_engraver::listen_note (Stream_event *ev)
{
  notes_.push_back (ev);
}

IMPLEMENT_TRANSLATOR_LISTENER (Chord_name_engraver, rest);
void
Chord_name_engraver::listen_rest (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE(rest_event_, ev);
}
  
void
Chord_name_engraver::stop_translation_timestep ()
{
  chord_name_ = 0;
  notes_.clear ();
  rest_event_ = 0;
}

/*
  The READs description is not strictly accurate:
  which properties are read depend on the chord naming function active.
*/
ADD_TRANSLATOR (Chord_name_engraver,
		/* doc */
		"Catch note and rest events and generate the appropriate chordname.",

		/* create */
		"ChordName ",

		/* read */
		"chordChanges "
		"chordNameExceptions "
		"chordNameFunction "
		"chordNoteNamer "
		"chordRootNamer "
		"chordNameExceptions "
		"majorSevenSymbol "
                "noChordSymbol ",

		/* write */
		""
		);
