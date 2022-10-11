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

#include "item.hh"
#include "melody-spanner.hh"
#include "pointer-group-interface.hh"

class Melody_engraver final : public Engraver
{
  Grob *melody_item_ = nullptr;
  Grob *next_melody_item_ = nullptr;
  // This engraver is designed to operate in Voice context, so we expect only
  // one stem.
  Grob *stem_ = nullptr;
  bool break_melody_ = true;

protected:
  void acknowledge_stem (Grob_info);
  void acknowledge_slur (Grob_info);
  TRANSLATOR_DECLARATIONS (Melody_engraver);
  void stop_translation_timestep ();
};

Melody_engraver::Melody_engraver (Context *c)
  : Engraver (c)
{
}

void
Melody_engraver::stop_translation_timestep ()
{
  if (stem_)
    {
      // If we don't already know a reason to start a new melody span, check
      // whether there is a bar line.  We can't use acknowledge_bar_line () for
      // this because the Bar_engraver operates in Staff context, so this
      // engraver can't observe its grobs.
      if (!break_melody_)
        break_melody_ = unsmob<Grob> (get_property (this, "currentBarLine"));

      if (break_melody_)
        {
          break_melody_ = false;

          melody_item_ = next_melody_item_;
          next_melody_item_ = nullptr;
        }

      Melody_spanner::add_stem (melody_item_, stem_);
      stem_ = nullptr;
    }
}

void
Melody_engraver::acknowledge_slur (Grob_info /* info */)
{
  break_melody_ = true;
}

void
Melody_engraver::acknowledge_stem (Grob_info info)
{
  auto *const stem = info.grob ();
  if (scm_is_false (get_property (this, "suspendMelodyDecisions")))
    {
      extract_grob_set (stem, "rests", rests);
      if (rests.empty ())
        {
          stem_ = stem;

          // We don't necessarily know yet whether we will need to place this
          // stem in a new melody span.  Create a next MelodyItem now because
          // creating grobs in stop_translation_timestep () isn't allowed.
          if (!next_melody_item_)
            next_melody_item_ = make_item ("MelodyItem", stem->self_scm ());
        }
      else
        break_melody_ = true;
    }
}

#include "translator.icc"

void
Melody_engraver::boot ()
{
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (slur);
}

ADD_TRANSLATOR (Melody_engraver,
                /* doc */
                R"(
Create information for context dependent typesetting decisions.
                )",

                /* create */
                R"(
MelodyItem
                )",

                /* read */
                R"(
currentBarLine
suspendMelodyDecisions
                )",

                /* write */
                R"(

                )");
