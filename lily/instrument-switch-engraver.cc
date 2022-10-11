/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>


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
#include "text-interface.hh"

#include "translator.icc"

class Instrument_switch_engraver : public Engraver
{

  TRANSLATOR_DECLARATIONS (Instrument_switch_engraver);

protected:
  Grob *text_;
  SCM cue_name_;

  void stop_translation_time_step ();
  void process_music ();

  void derived_mark () const override;
};

void
Instrument_switch_engraver::derived_mark () const
{
  scm_gc_mark (cue_name_);
}

Instrument_switch_engraver::Instrument_switch_engraver (Context *c)
  : Engraver (c)
{
  cue_name_ = SCM_EOL;
  text_ = 0;
}

/*
  TODO: should use an event.
 */
void
Instrument_switch_engraver::process_music ()
{
  SCM cue_text = get_property (this, "instrumentCueName");

  if (!scm_is_eq (cue_name_, cue_text))
    {
      if (Text_interface::is_markup (cue_text))
        {
          text_ = make_item ("InstrumentSwitch", SCM_EOL);
          set_property (text_, "text", cue_text);
        }
      cue_name_ = cue_text;
    }
}

void
Instrument_switch_engraver::stop_translation_time_step ()
{
  text_ = 0;
}

void
Instrument_switch_engraver::boot ()
{
}

ADD_TRANSLATOR (Instrument_switch_engraver,
                /* doc */
                R"(
Create a cue text for taking instrument.

This engraver is deprecated.
                )",

                /* create */
                R"(
InstrumentSwitch
                )",

                /* read */
                R"(
instrumentCueName
                )",

                /* write */
                R"(

                )");
