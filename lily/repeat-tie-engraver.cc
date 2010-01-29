/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>


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
#include "item.hh"
#include "pointer-group-interface.hh"
#include "stream-event.hh"

#include "translator.icc"

class Repeat_tie_engraver : public Engraver
{
  Stream_event *event_;
  Grob *semi_tie_column_;
  vector<Grob*> semi_ties_;
  
  void stop_translation_timestep (); 
  DECLARE_ACKNOWLEDGER (note_head);
  DECLARE_TRANSLATOR_LISTENER (repeat_tie);
  
public:
  TRANSLATOR_DECLARATIONS (Repeat_tie_engraver);
};

Repeat_tie_engraver::Repeat_tie_engraver ()
{
  event_ = 0;
  semi_tie_column_ = 0;
}

void
Repeat_tie_engraver::stop_translation_timestep ()
{
  event_ = 0;
  semi_tie_column_ = 0;
  semi_ties_.clear ();
}

IMPLEMENT_TRANSLATOR_LISTENER (Repeat_tie_engraver, repeat_tie);
void
Repeat_tie_engraver::listen_repeat_tie (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (event_, ev);
}

void
Repeat_tie_engraver::acknowledge_note_head (Grob_info inf)
{
  if (!event_)
    return;

  if (!semi_tie_column_)
    {
      semi_tie_column_ = make_item ("RepeatTieColumn", event_->self_scm ());
    }

  SCM cause = event_->self_scm ();
  Grob *semi_tie = make_item ("RepeatTie", cause);
  semi_tie->set_object ("note-head", inf.grob ()->self_scm ());
  
  Pointer_group_interface::add_grob (semi_tie_column_, ly_symbol2scm ("ties"),
				     semi_tie);
  semi_tie->set_parent (semi_tie_column_, Y_AXIS);
  semi_ties_.push_back (semi_tie);


  if (is_direction (unsmob_stream_event (cause)->get_property ("direction")))
    {
      Direction d = to_dir (unsmob_stream_event (cause)->get_property ("direction"));
      semi_tie->set_property ("direction", scm_from_int (d)); 
    }

}

ADD_ACKNOWLEDGER (Repeat_tie_engraver, note_head);
ADD_TRANSLATOR (Repeat_tie_engraver, 
		/* doc */
		"Create repeat ties.",
		
		/* create */
		"RepeatTie "
		"RepeatTieColumn ",

		/* read */
		"",

		/* write */
		""
		);
