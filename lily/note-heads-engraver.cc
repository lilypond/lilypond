/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "duration.hh"
#include "item.hh"
#include "output-def.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include <cctype>

#include "translator.icc"

using std::vector;

class Note_heads_engraver : public Engraver
{
  vector<Stream_event *> note_evs_;

public:
  TRANSLATOR_DECLARATIONS (Note_heads_engraver);

protected:
  void listen_note (Stream_event *);
  void process_music ();
  void stop_translation_timestep ();
};

Note_heads_engraver::Note_heads_engraver (Context *c)
  : Engraver (c)
{
}

void
Note_heads_engraver::listen_note (Stream_event *ev)
{
  note_evs_.push_back (ev);
}

void
Note_heads_engraver::process_music ()
{
  SCM c0 = get_property (this, "middleCPosition");
  SCM layout_proc = get_property (this, "staffLineLayoutFunction");

  for (vsize i = 0; i < note_evs_.size (); i++)
    {
      Stream_event *ev = note_evs_[i];
      Item *note = make_item ("NoteHead", ev->self_scm ());

      Pitch *pit = unsmob<Pitch> (get_property (ev, "pitch"));

#if 0 /* TODO: should have a mechanism to switch off these warnings. */

      if (!pit)
        ev->warning (_ ("NoteEvent without pitch"));
#endif

      int pos;
      if (pit == 0)
        pos = 0;
      else if (ly_is_procedure (layout_proc))
        {
          SCM pitch = get_property (ev, "pitch");
          pos = from_scm<int> (ly_call (layout_proc, pitch));
        }
      else
        pos = pit->steps ();

      if (scm_is_number (c0))
        pos += from_scm<int> (c0);

      set_property (note, "staff-position", to_scm (pos));

      /*
        Shape note heads change on step of the scale.
      */
      SCM shape_vector = get_property (this, "shapeNoteStyles");
      if (scm_is_vector (shape_vector))
        {
          SCM scm_tonic = get_property (this, "tonic");
          Pitch tonic;
          if (unsmob<Pitch> (scm_tonic))
            tonic = *unsmob<Pitch> (scm_tonic);

          unsigned int delta
            = (pit->get_notename () - tonic.get_notename () + 7) % 7;

          SCM style = SCM_EOL;
          if (scm_c_vector_length (shape_vector) > delta
              && scm_is_symbol (scm_vector_ref (shape_vector, to_scm (delta))))
            style = scm_vector_ref (shape_vector, to_scm (delta));
          if (scm_is_symbol (style))
            set_property (note, "style", style);
        }
    }
}

void
Note_heads_engraver::stop_translation_timestep ()
{
  note_evs_.clear ();
}

void
Note_heads_engraver::boot ()
{
  ADD_LISTENER (note);
}

ADD_TRANSLATOR (Note_heads_engraver,
                /* doc */
                R"(
Generate note heads.
                )",

                /* create */
                R"(
NoteHead
                )",

                /* read */
                R"(
middleCPosition
staffLineLayoutFunction
                )",

                /* write */
                R"(

                )");
