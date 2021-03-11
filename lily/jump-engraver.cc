/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2018--2021 Dan Eble <dan@faithful.be>

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
#include "stream-event.hh"
#include "text-interface.hh"
#include "warn.hh"

#include "translator.icc"

/**
   Create marks such as "D.C. al Fine" outside the system.
*/
class Jump_engraver final : public Engraver
{
  Item *text_ = nullptr;
  Stream_event *fine_ev_ = nullptr;

public:
  TRANSLATOR_DECLARATIONS (Jump_engraver);

protected:
  void process_music ();
  void stop_translation_timestep ();

  void listen_fine (Stream_event *);
};

Jump_engraver::Jump_engraver (Context *c)
  : Engraver (c)
{
}

void
Jump_engraver::listen_fine (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (fine_ev_, ev);
}

void
Jump_engraver::process_music ()
{
  SCM m = SCM_EOL;
  Stream_event *ev = nullptr;

  if (fine_ev_)
    {
      ev = fine_ev_;
      text_ = make_item ("JumpScript", ev->self_scm ());
      m = get_property (this, "fineText");
    }

  if (ev)
    {
      if (Text_interface::is_markup (m))
        set_property (text_, "text", m);
      else
        ev->warning (_ ("jump text must be a markup object"));
    }
}

void
Jump_engraver::stop_translation_timestep ()
{
  if (text_)
    {
      set_object (text_, "side-support-elements",
                  grob_list_to_grob_array (get_property (this, "stavesFound")));
      text_ = nullptr;
    }

  fine_ev_ = nullptr;
}

void
Jump_engraver::boot ()
{
  ADD_LISTENER (Jump_engraver, fine);
}

ADD_TRANSLATOR (Jump_engraver,
                /* doc */
                "Create @code{JumpScript} objects.  It puts them outside"
                " all staves (which is taken from the property"
                " @code{stavesFound}).  If moving this engraver to a different"
                " context, @ref{Staff_collecting_engraver} must move along,"
                " otherwise all marks end up on the same Y@tie{}location.",

                /* create */
                "JumpScript ",

                /* read */
                "stavesFound ",

                /* write */
                ""
               );
