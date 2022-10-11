/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "context.hh"
#include "engraver.hh"
#include "note-head.hh"
#include "lyric-extender.hh"
#include "pointer-group-interface.hh"
#include "side-position-interface.hh"

#include "translator.icc"

using std::vector;

class Stanza_number_align_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Stanza_number_align_engraver);

protected:
  vector<Grob *> lyrics_;
  vector<Grob *> stanza_numbers_;

  void acknowledge_lyric_syllable (Grob_info);
  void acknowledge_stanza_number (Grob_info);
  void stop_translation_timestep ();
};

Stanza_number_align_engraver::Stanza_number_align_engraver (Context *c)
  : Engraver (c)
{
}

void
Stanza_number_align_engraver::acknowledge_lyric_syllable (Grob_info gi)
{
  Grob *h = gi.grob ();
  lyrics_.push_back (h);
}

void
Stanza_number_align_engraver::acknowledge_stanza_number (Grob_info gi)
{
  Grob *h = gi.grob ();
  stanza_numbers_.push_back (h);
}

void
Stanza_number_align_engraver::stop_translation_timestep ()
{
  for (vsize i = lyrics_.size (); i--;)
    for (vsize j = stanza_numbers_.size (); j--;)
      Side_position_interface::add_support (stanza_numbers_[j], lyrics_[i]);

  stanza_numbers_.clear ();
  lyrics_.clear ();
}

void
Stanza_number_align_engraver::boot ()
{
  ADD_ACKNOWLEDGER (lyric_syllable);
  ADD_ACKNOWLEDGER (stanza_number);
}

ADD_TRANSLATOR (Stanza_number_align_engraver,
                /* doc */
                R"(
This engraver ensures that stanza numbers are neatly aligned.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
