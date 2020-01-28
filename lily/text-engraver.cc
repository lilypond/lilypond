/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "directional-element-interface.hh"
#include "engraver.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

using std::vector;

/**
   typeset directions that are  plain text.
*/

class Text_engraver : public Engraver
{
  vector<Stream_event *> evs_;
  vector<Grob *> scripts_;

public:
  TRANSLATOR_DECLARATIONS (Text_engraver);

protected:
  void stop_translation_timestep ();
  void process_music ();

  void acknowledge_note_column (Grob_info);
  void listen_text_script (Stream_event *);
};

void
Text_engraver::listen_text_script (Stream_event *ev)
{
  evs_.push_back (ev);
}

void
Text_engraver::process_music ()
{
  for (vsize i = 0; i < evs_.size (); i++)
    {
      Stream_event *ev = evs_[i];

      Item *script = make_item ("TextScript", ev->self_scm ());
      scripts_.push_back (script);

      int priority
          = robust_scm2int (script->get_property ("script-priority"), 200);

      /* see script-engraver.cc */
      priority += i;

      script->set_property ("script-priority", scm_from_int (priority));

      Direction dir = to_dir (ev->get_property ("direction"));
      if (dir)
        set_grob_direction (script, dir);

      SCM mark = ev->get_property ("text");

      script->set_property ("text", mark);
    }
}

void
Text_engraver::acknowledge_note_column (Grob_info info)
{
  // Make note column (or rest, if there are no heads) the parent of the script.
  extract_grob_set (info.grob (), "note-heads", heads);
  Grob *x_parent
      = (heads.size () ? info.grob ()
                       : unsmob<Grob> (info.grob ()->get_object ("rest")));

  for (vsize i = 0; i < scripts_.size (); i++)
    {
      Grob *el = scripts_[i];

      if (el && !el->get_parent (X_AXIS) && x_parent)
        el->set_parent (x_parent, X_AXIS);
    }
}

void
Text_engraver::stop_translation_timestep ()
{
  evs_.clear ();
  scripts_.clear ();
}

Text_engraver::Text_engraver (Context *c) : Engraver (c) {}

void
Text_engraver::boot ()
{
  ADD_LISTENER (Text_engraver, text_script);
  ADD_ACKNOWLEDGER (Text_engraver, note_column);
}

ADD_TRANSLATOR (Text_engraver,
                /* doc */
                "Create text scripts.",

                /* create */
                "TextScript ",

                /* read */
                "",

                /* write */
                "");
