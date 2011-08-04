/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2011 Bertrand Bordage
                     Mike Solomon

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

#include "pointer-group-interface.hh"
#include "arpeggio.hh"
#include "stem.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "stream-event.hh"
#include "note-column.hh"
#include "item.hh"

#include "translator.icc"

class Brace_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Brace_engraver);

  void acknowledge_stem (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
protected:
  void process_music ();
  void stop_translation_timestep ();
  DECLARE_TRANSLATOR_LISTENER (brace);

private:
  Item *brace_;
  Stream_event *brace_event_;
};

Brace_engraver::Brace_engraver ()
{
  brace_ = 0;
  brace_event_ = 0;
}

IMPLEMENT_TRANSLATOR_LISTENER (Brace_engraver, brace);
void Brace_engraver::listen_brace (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (brace_event_, ev);
}

void
Brace_engraver::acknowledge_stem (Grob_info info)
{
  if (brace_)
    {
      if (!brace_->get_parent (Y_AXIS))
        brace_->set_parent (info.grob (), Y_AXIS);

      Pointer_group_interface::add_grob (brace_,
                                         ly_symbol2scm ("stems"),
                                         info.grob ());
    }
}
void
Brace_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  if (brace_)

    /*
      We can't catch local key items (accidentals) from Voice context,
      see Local_key_engraver
    */
    Side_position_interface::add_support (brace_, info.grob ());
}

void
Brace_engraver::process_music ()
{
  if (brace_event_)
    brace_ = make_item ("Brace", brace_event_->self_scm ());
}

void
Brace_engraver::stop_translation_timestep ()
{
  brace_ = 0;
  brace_event_ = 0;
}

ADD_ACKNOWLEDGER (Brace_engraver, stem);
ADD_ACKNOWLEDGER (Brace_engraver, rhythmic_head);

ADD_TRANSLATOR (Brace_engraver,
                /* doc */
                "Generate a Brace symbol.",

                /* create */
                "Brace",

                /* read */
                "",

                /* write */
                ""
                );

