/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>, Glen Prideaux <glenprideaux@iname.com>

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
#include "side-position-interface.hh"
#include "text-interface.hh"
#include "item.hh"

class Stanza_number_engraver : public Engraver
{
  Item *text_;

  SCM last_stanza_;

public:
  TRANSLATOR_DECLARATIONS (Stanza_number_engraver);
  void process_music ();
  void derived_mark () const override;
  void stop_translation_timestep ();
  void acknowledge_lyric_syllable (Grob_info);
};

void
Stanza_number_engraver::derived_mark () const
{
  scm_gc_mark (last_stanza_);
}

/*
  TODO: should make engraver that collects all the stanzas on a higher
  level, and then groups them to the side. Stanza numbers should be
  all aligned.
*/

Stanza_number_engraver::Stanza_number_engraver (Context *c)
  : Engraver (c)
{
  text_ = 0;
  last_stanza_ = SCM_EOL;
}

void
Stanza_number_engraver::process_music ()
{
  SCM stanza = get_property (this, "stanza");

  if (Text_interface::is_markup (stanza) && !scm_is_eq (stanza, last_stanza_))
    {
      last_stanza_ = stanza;

      text_ = make_item ("StanzaNumber", SCM_EOL);
      set_property (text_, "text", stanza);
    }
}

void
Stanza_number_engraver::acknowledge_lyric_syllable (Grob_info inf)
{
  if (text_)
    Side_position_interface::add_support (text_, inf.grob ());
}

void
Stanza_number_engraver::stop_translation_timestep ()
{
  text_ = 0;
}

#include "translator.icc"

void
Stanza_number_engraver::boot ()
{
  ADD_ACKNOWLEDGER (lyric_syllable);
}

ADD_TRANSLATOR (Stanza_number_engraver,
                /* doc */
                R"(
Engrave stanza numbers.
                )",

                /* create */
                R"(
StanzaNumber
                )",

                /* read */
                R"(
stanza
                )",

                /* write */
                R"(

                )");
