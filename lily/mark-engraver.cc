/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2021 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "mark-tracking-translator.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

#include <cctype>

/**
   put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
   rehearsal marks.
*/
class Mark_engraver : public Engraver
{
  Item *text_ = nullptr;
  Item *final_text_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Mark_engraver);

protected:
  void process_music ();
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void finalize () override;
};

Mark_engraver::Mark_engraver (Context *c)
  : Engraver (c)
{
}

void
Mark_engraver::start_translation_timestep ()
{
  final_text_ = nullptr;
}

void
Mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      final_text_ = text_;
      text_ = nullptr;
    }
}

void
Mark_engraver::finalize ()
{
  if (final_text_)
    set_property (final_text_, "break-visibility",
                  scm_c_make_vector (3, SCM_BOOL_T));
  final_text_ = nullptr;
}

void
Mark_engraver::process_music ()
{
  if (text_)
    return;

  // Get the event chosen by Mark_tracking_translator.
  SCM ev_scm = get_property (this, "currentMarkEvent");
  auto *const ev = unsmob<Stream_event> (ev_scm);
  if (!ev)
    return;

  SCM text = SCM_EOL;
  if (ev->in_event_class ("rehearsal-mark-event"))
    {
      const auto label
        = Mark_tracking_translator::get_rehearsal_mark_label (context (), ev);
      if (label > 0)
        {
          SCM proc = get_property (this, "markFormatter");
          if (ly_is_procedure (proc))
            text = scm_call_2 (proc, to_scm (label), context ()->self_scm ());
        }
    }
  else // ad-hoc-mark-event
    {
      text = get_property (ev, "text");
    }

  text_ = make_item ("RehearsalMark", ev->self_scm ());
  if (Text_interface::is_markup (text))
    set_property (text_, "text", text);
  else
    ev->warning (_ ("mark label must be a markup object"));
}

void
Mark_engraver::boot ()
{
}

ADD_TRANSLATOR (Mark_engraver,
                /* doc */ R"(

This engraver creates rehearsal marks.

@code{Mark_@/engraver} creates marks formatted according to the
@code{markFormatter} context property and places them vertically outside the
set of staves given in the @code{stavesFound} context property.

If @code{Mark_@/engraver} is added or moved to another context,
@iref{Staff_collecting_engraver} also needs to be there so that marks appear at
the intended Y@tie{}location.

By default, @code{Mark_@/engravers} in multiple contexts create a common
sequence of marks chosen by the @code{Score}-level
@iref{Mark_tracking_translator}.  If independent sequences are desired,
multiple @code{Mark_tracking_translators} must be used.

)",

                /* create */
                "RehearsalMark ",

                /* read */
                "currentMarkEvent "
                "markFormatter "
                "stavesFound ",

                /* write */
                ""
);
