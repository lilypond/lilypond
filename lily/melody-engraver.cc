/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

class Melody_engraver : public Engraver
{
  Grob *melody_item_ = nullptr;
  Grob *stem_ = nullptr;
protected:

  void acknowledge_stem (Grob_info);
  void acknowledge_slur (Grob_info);
  TRANSLATOR_DECLARATIONS (Melody_engraver);
  void stop_translation_timestep ();
  void process_acknowledged ();
  void process_music ();
};

Melody_engraver::Melody_engraver (Context *c)
  : Engraver (c)
{
}

void
Melody_engraver::process_music ()
{
  if (scm_is_string (get_property (this, "whichBar")))
    melody_item_ = nullptr;
}

/*
  Used to be in stop_translation_timestep, but grobs can't
  be created here.
*/
void
Melody_engraver::process_acknowledged ()
{
  if (stem_
      && !is_scm<Direction> (get_property_data (stem_, "neutral-direction")))
    {
      extract_grob_set (stem_, "rests", rests);
      if (rests.size ())
        melody_item_ = nullptr;
      else
        {
          if (!melody_item_)
            melody_item_ = make_item ("MelodyItem", stem_->self_scm ());

          Melody_spanner::add_stem (melody_item_, stem_);
        }
    }
}

void
Melody_engraver::stop_translation_timestep ()
{
  stem_ = nullptr;
}

void
Melody_engraver::acknowledge_slur (Grob_info /* info */)
{
  melody_item_ = nullptr;
}

void
Melody_engraver::acknowledge_stem (Grob_info info)
{
  stem_ = info.grob ();
}

#include "translator.icc"

void
Melody_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Melody_engraver, stem);
  ADD_ACKNOWLEDGER (Melody_engraver, slur);
}

ADD_TRANSLATOR (Melody_engraver,
                /* doc */
                "Create information for context dependent typesetting"
                " decisions.",

                /* create */
                "MelodyItem ",

                /* read */
                "",

                /* write */
                ""
               );

