/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2023 Jan Nieuwenhuizen <janneke@gnu.org>

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

#include "arpeggio.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "separation-item.hh"
#include "side-position-interface.hh"
#include "stem.hh"
#include "stream-event.hh"

#include "translator.icc"

namespace
{

enum class Arpeggio_type
{
  NON_ARPEGGIATED = 0,
  SLURRED,
  ARPEGGIATED,
};

}

class Arpeggio_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Arpeggio_engraver);

  void acknowledge_stem (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);

protected:
  void process_music ();
  void stop_translation_timestep ();
  void listen_arpeggio (Stream_event *);
  void listen_chord_slur (Stream_event *);
  void listen_non_arpeggiato (Stream_event *);

private:
  Item *arpeggio_ = nullptr;
  Stream_event *arpeggio_event_ = nullptr;
  Arpeggio_type arpeggio_type_ = Arpeggio_type::NON_ARPEGGIATED;
};

Arpeggio_engraver::Arpeggio_engraver (Context *c)
  : Engraver (c)
{
}

void
Arpeggio_engraver::listen_arpeggio (Stream_event *ev)
{
  if (assign_event_once (arpeggio_event_, ev))
    {
      arpeggio_type_ = Arpeggio_type::ARPEGGIATED;
    }
}

void
Arpeggio_engraver::listen_chord_slur (Stream_event *ev)
{
  if (assign_event_once (arpeggio_event_, ev))
    {
      arpeggio_type_ = Arpeggio_type::SLURRED;
    }
}

void
Arpeggio_engraver::listen_non_arpeggiato (Stream_event *ev)
{
  if (assign_event_once (arpeggio_event_, ev))
    {
      arpeggio_type_ = Arpeggio_type::NON_ARPEGGIATED;
    }
}

void
Arpeggio_engraver::acknowledge_stem (Grob_info info)
{
  if (arpeggio_)
    {
      if (!arpeggio_->get_y_parent ())
        arpeggio_->set_y_parent (info.grob ());

      Pointer_group_interface::add_grob (arpeggio_, ly_symbol2scm ("stems"),
                                         info.grob ());
    }
}
void
Arpeggio_engraver::acknowledge_rhythmic_head (Grob_info info)
{
  if (arpeggio_)

    /*
      We can't catch local key items (accidentals) from Voice context,
      see Local_key_engraver
    */
    Side_position_interface::add_support (arpeggio_, info.grob ());
}

void
Arpeggio_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  if (arpeggio_)
    Separation_item::add_conditional_item (info.grob (), arpeggio_);
}

void
Arpeggio_engraver::process_music ()
{
  if (arpeggio_event_)
    {
      const char *const item_name = [this] {
        switch (arpeggio_type_)
          {
            // clang-format off
          case Arpeggio_type::NON_ARPEGGIATED: return "ChordBracket";
          case Arpeggio_type::SLURRED: return "ChordSlur";
          case Arpeggio_type::ARPEGGIATED: return "Arpeggio";
            // clang-format on
          }
        abort ();
      }();
      arpeggio_ = make_item (item_name, arpeggio_event_->self_scm ());
    }
}

void
Arpeggio_engraver::stop_translation_timestep ()
{
  arpeggio_ = nullptr;
  arpeggio_event_ = nullptr;
}

void
Arpeggio_engraver::boot ()
{
  ADD_LISTENER (arpeggio);
  ADD_LISTENER (chord_slur);
  ADD_LISTENER (non_arpeggiato);
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (rhythmic_head);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Arpeggio_engraver,
                /* doc */
                R"(
Create arpeggiato and non-arpeggiato symbols.
                )",

                /* create */
                R"(
Arpeggio
ChordBracket
ChordSlur
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
