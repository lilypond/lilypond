/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2023 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"

#include "translator.icc"

/**
   Make arpeggios that span multiple staves.  Catch arpeggios, and span a
   Span_arpeggio over them if we find more than two arpeggios.
*/
class Span_arpeggio_engraver final : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Span_arpeggio_engraver);
  void acknowledge_arpeggio (Grob_info_t<Item>);
  void acknowledge_chord_bracket (Grob_info_t<Item>);
  void acknowledge_chord_slur (Grob_info_t<Item>);
  void acknowledge_note_column (Grob_info_t<Item>);

protected:
  void process_acknowledged ();
  void stop_translation_timestep ();

private:
  struct Chord_onset_info
  {
    const char *const item_name_;
    const char *const connect_property_name_;
    std::vector<Item *> items_ {};
    Item *span_item_ = nullptr;
  };

private:
  Chord_onset_info arpeggio_ = {"Arpeggio", "connectArpeggios"};
  Chord_onset_info bracket_ = {"ChordBracket", "connectChordBrackets"};
  Chord_onset_info slur_ = {"ChordSlur", "connectChordSlurs"};
  std::vector<Grob *> note_columns_;
};

Span_arpeggio_engraver::Span_arpeggio_engraver (Context *c)
  : Engraver (c)
{
}

void
Span_arpeggio_engraver::acknowledge_arpeggio (Grob_info_t<Item> info)
{
  arpeggio_.items_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::acknowledge_chord_bracket (Grob_info_t<Item> info)
{
  bracket_.items_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::acknowledge_chord_slur (Grob_info_t<Item> info)
{
  slur_.items_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  note_columns_.push_back (info.grob ());
}

void
Span_arpeggio_engraver::process_acknowledged ()
{
  for (auto *const info : {&arpeggio_, &bracket_, &slur_})
    {
      /*
        connectArpeggios is slightly brusque; we should really read a grob
        property of the caught non-span arpeggios. That way, we can have

        both non-connected and connected arps in one pianostaff.

      */
      if (!info->span_item_ && info->items_.size () > 1
          && from_scm<bool> (get_property (this, info->connect_property_name_)))
        info->span_item_ = make_item (info->item_name_, SCM_EOL);

      if (info->span_item_)
        {
          for (auto *const col : note_columns_)
            {
              Separation_item::add_conditional_item (col, info->span_item_);
            }
          note_columns_.clear ();
        }
    }
}

void
Span_arpeggio_engraver::stop_translation_timestep ()
{
  for (auto *const info : {&arpeggio_, &bracket_, &slur_})
    {
      if (info->span_item_)
        {
          /*
            we do this very late, to make sure we also catch `extra'
            side-pos support like accidentals.
          */
          for (auto *const item : info->items_)
            {
              extract_grob_set (item, "stems", stems);
              for (auto *const stem : stems)
                {
                  Pointer_group_interface::add_grob (
                    info->span_item_, ly_symbol2scm ("stems"), stem);
                }

              extract_grob_set (item, "side-support-elements", sses);
              for (auto *const sse : sses)
                {
                  Pointer_group_interface::add_grob (
                    info->span_item_, ly_symbol2scm ("side-support-elements"),
                    sse);
                }

              /*
                we can't kill the children, since we don't want to the
                previous note to bump into the span arpeggio; so we make
                it transparent.
              */
              set_object (item, "vertically-spanning-surrogate",
                          to_scm (info->span_item_));
              set_property (item, "transparent", SCM_BOOL_T);
            }

          info->span_item_->set_y_parent (info->items_[0]->get_y_parent ());
          info->span_item_ = nullptr;
        }
    }
  arpeggio_.items_.clear ();
  bracket_.items_.clear ();
  slur_.items_.clear ();
  note_columns_.clear ();
}

void
Span_arpeggio_engraver::boot ()
{
  ADD_ACKNOWLEDGER (arpeggio);
  ADD_ACKNOWLEDGER (chord_bracket);
  ADD_ACKNOWLEDGER (chord_slur);
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Span_arpeggio_engraver,
                /* doc */
                R"(
Make arpeggios, non-arpeggiato brackets, and vertical slurs spanning multiple
staves.
                )",

                /* create */
                R"(
Arpeggio
ChordBracket
ChordSlur
                )",

                /* read */
                R"(
connectArpeggios
connectChordBrackets
connectChordSlurs
                )",

                /* write */
                R"(

                )");
