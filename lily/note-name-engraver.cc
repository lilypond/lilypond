/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1999--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "engraver.hh"
#include "item.hh"
#include "lily-imports.hh"
#include "pitch.hh"
#include "stream-event.hh"
#include "text-interface.hh"

#include "translator.icc"

using std::string;
using std::vector;

class Note_name_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Note_name_engraver);

  vector<Stream_event *> events_;
  void listen_note (Stream_event *);
  void process_music ();
  void stop_translation_timestep ();
};

void
Note_name_engraver::listen_note (Stream_event *ev)
{
  events_.push_back (ev);
}

void
Note_name_engraver::process_music ()
{
  SCM markup_list = SCM_EOL;

  for (vsize i = 0; i < events_.size (); i++)
    {
      SCM pitch = events_[i]->get_property ("pitch");
      SCM proc = get_property ("noteNameFunction");
      SCM sep = get_property ("noteNameSeparator");

      if (i)
        markup_list = scm_append (scm_list_2 (
            scm_list_1 (Text_interface::is_markup (sep) ? sep
                                                        : ly_string2scm (" ")),
            markup_list));

      if (ly_is_procedure (proc))
        {
          SCM pitch_name = scm_call_2 (proc, pitch, context ()->self_scm ());
          markup_list
              = scm_append (scm_list_2 (scm_list_1 (pitch_name), markup_list));
        }
      else
        programming_error (
            "No translation function defined as noteNameFunction.");
    }
  if (!scm_is_null (markup_list))
    {
      Item *n = make_item ("NoteName", events_[0]->self_scm ());
      SCM text = Lily::make_concat_markup (scm_reverse (markup_list));
      n->set_property ("text", text);
    }
}

void
Note_name_engraver::stop_translation_timestep ()
{
  events_.clear ();
}

Note_name_engraver::Note_name_engraver (Context *c) : Engraver (c) {}

void
Note_name_engraver::boot ()
{
  ADD_LISTENER (Note_name_engraver, note);
}

ADD_TRANSLATOR (Note_name_engraver,
                /* doc */
                "Print pitches as words.",

                /* create */
                "NoteName ",

                /* read */
                "noteNameFunction "
                "noteNameSeparator "
                "printAccidentalNames "
                "printNotesLanguage "
                "printOctaveNames ",

                /* write */
                "");
