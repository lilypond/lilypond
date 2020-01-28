/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include <cctype>

#include "engraver.hh"

#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "international.hh"
#include "item.hh"
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

/**
   put stuff over or next to  bars.  Examples: bar numbers, marginal notes,
   rehearsal marks.
*/
class Mark_engraver : public Engraver
{

  void create_items (Stream_event *);
  Item *text_;
  Item *final_text_;
  Stream_event *mark_ev_;

public:
  TRANSLATOR_DECLARATIONS (Mark_engraver);

protected:
  void process_music ();
  void start_translation_timestep ();
  void stop_translation_timestep ();
  void finalize () override;

  void listen_mark (Stream_event *);
  void acknowledge_break_alignment (Grob_info);
};

Mark_engraver::Mark_engraver (Context *c) : Engraver (c)
{
  text_ = 0;
  final_text_ = 0;
  mark_ev_ = 0;
}

void
Mark_engraver::acknowledge_break_alignment (Grob_info inf)
{
  Grob *s = inf.grob ();
  if (text_ && dynamic_cast<Item *> (s))
    text_->set_parent (s, X_AXIS);
}

void
Mark_engraver::start_translation_timestep ()
{
  final_text_ = 0;
}

void
Mark_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      text_->set_object (
          "side-support-elements",
          grob_list_to_grob_array (get_property ("stavesFound")));
      final_text_ = text_;
      text_ = 0;
    }
  mark_ev_ = 0;
}

void
Mark_engraver::finalize ()
{
  if (final_text_)
    final_text_->set_property ("break-visibility",
                               scm_c_make_vector (3, SCM_BOOL_T));
  final_text_ = 0;
}

void
Mark_engraver::create_items (Stream_event *ev)
{
  if (text_)
    return;

  text_ = make_item ("RehearsalMark", ev->self_scm ());
}

void
Mark_engraver::listen_mark (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (mark_ev_, ev);
}

/*
  TODO: make the increment function in Scheme.
*/
void
Mark_engraver::process_music ()
{
  if (mark_ev_)
    {
      create_items (mark_ev_);

      /*
        automatic marks.
      */

      SCM m = mark_ev_->get_property ("label");
      SCM proc = get_property ("markFormatter");
      if (!Text_interface::is_markup (m) && ly_is_procedure (proc))
        {
          if (!scm_is_number (m))
            m = get_property ("rehearsalMark");

          if (scm_is_number (m))
            {
              context ()->set_property ("rehearsalMark", scm_oneplus (m));
              m = scm_call_2 (proc, m, context ()->self_scm ());
            }
          else
            /* Score.rehearsalMark is initialized to #1 so we
               never should see this case without user error */
            mark_ev_->origin ()->warning (
                _ ("rehearsalMark must have integer value"));
        }

      if (Text_interface::is_markup (m))
        text_->set_property ("text", m);
      else
        mark_ev_->origin ()->warning (_ ("mark label must be a markup object"));
    }
}

void
Mark_engraver::boot ()
{
  ADD_LISTENER (Mark_engraver, mark);
  ADD_ACKNOWLEDGER (Mark_engraver, break_alignment);
}

ADD_TRANSLATOR (Mark_engraver,
                /* doc */
                "Create @code{RehearsalMark} objects.  It puts them on top of"
                " all staves (which is taken from the property"
                " @code{stavesFound}).  If moving this engraver to a different"
                " context, @ref{Staff_collecting_engraver} must move along,"
                " otherwise all marks end up on the same Y@tie{}location.",

                /* create */
                "RehearsalMark ",

                /* read */
                "markFormatter "
                "rehearsalMark "
                "stavesFound ",

                /* write */
                "");
