/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2014 Jan Nieuwenhuizen <janneke@gnu.org>

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
  DECLARE_TRANSLATOR_LISTENER (note);
  DECLARE_TRANSLATOR_LISTENER (rest);
private:
  vector<Stream_event *> notes_;

  Stream_event *rest_event_;
};

void
Chord_name_engraver::finalize ()
{
}

Chord_name_engraver::Chord_name_engraver ()
{
  rest_event_ = 0;
}

void
Chord_name_engraver::process_music ()
{
  if (!rest_event_ && notes_.empty ())
    return;

  SCM markup;
  SCM bass = SCM_EOL;
  SCM inversion = SCM_EOL;
  SCM pitches = SCM_EOL;
  Item *chord_name = 0;

  // rest events present a hen-and-egg problem with regard to
  // overriding the text property of the ChordName grob since we
  // cannot create a ChordName grob, look at its text property and, if
  // not set, use noChordSymbol to decide whether we should not have
  // created the grob in the first place.
  if (rest_event_)
    {
      SCM no_chord_markup = get_property ("noChordSymbol");
      if (!Text_interface::is_markup (no_chord_markup))
        return;
      markup = no_chord_markup;
      chord_name = make_item ("ChordName", rest_event_->self_scm ());
      chord_name->set_property ("text", markup);
    }
  else
    {
      chord_name = make_item ("ChordName", notes_[0]->self_scm ());
      // We cannot actually delay fetching the text property in case
      // it is a callback since we need to compare the generated
      // markups for the sake of chordChanges
      markup = chord_name->get_property ("text");
      if (!Text_interface::is_markup (markup))
        {
          for (vsize i = 0; i < notes_.size (); i++)
            {
              Stream_event *n = notes_[i];
              SCM p = n->get_property ("pitch");
              if (!Pitch::is_smob (p))
                continue;

              if (n->get_property ("bass") == SCM_BOOL_T)
                bass = p;
              else
                {
                  SCM oct = n->get_property ("octavation");
                  if (scm_is_number (oct))
                    {
                      Pitch orig = Pitch::unsmob (p)->transposed (Pitch (-scm_to_int (oct), 0));
                      pitches = scm_cons (orig.smobbed_copy (), pitches);
                    }
                  else
                    pitches = scm_cons (p, pitches);
                  if (n->get_property ("inversion") == SCM_BOOL_T)
                    {
                      inversion = p;
                      if (!scm_is_number (oct))
                        programming_error ("inversion does not have original pitch");
                    }
                }
            }

          pitches = scm_sort_list (pitches, Pitch::less_p_proc);

          SCM name_proc = get_property ("chordNameFunction");
          markup = scm_call_4 (name_proc, pitches, bass, inversion,
                               context ()->self_scm ());
          if (!Text_interface::is_markup (markup))
            {
              // Ugh, we created a grob, now we better populate it.
              // Use an empty string.
              markup = scm_string (SCM_EOL);
            }
          chord_name->set_property ("text", markup);
        }
    }

  SCM chord_changes = get_property ("chordChanges");
  SCM last_chord = get_property ("lastChord");
  if (to_boolean (chord_changes) && ly_is_equal (markup, last_chord))
    chord_name->set_property ("begin-of-line-visible", SCM_BOOL_T);

  context ()->set_property ("lastChord", markup);
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
  ASSIGN_EVENT_ONCE (rest_event_, ev);
}

void
Chord_name_engraver::stop_translation_timestep ()
{
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
                "lastChord "
                "majorSevenSymbol "
                "noChordSymbol ",

                /* write */
                "lastChord "
               );
