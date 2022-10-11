/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grob.hh"
#include "engraver.hh"

class Font_size_engraver : public Engraver
{

  TRANSLATOR_DECLARATIONS (Font_size_engraver);

protected:
  void acknowledge_font (Grob_info);
  void process_music ();
  Real size;

private:
};

Font_size_engraver::Font_size_engraver (Context *c)
  : Engraver (c)
{
  size = 0.0;
}

void
Font_size_engraver::process_music ()
{
  size = from_scm<double> (get_property (this, "fontSize"), 0.0);
}

void
Font_size_engraver::acknowledge_font (Grob_info gi)
{
  /*
    We only want to process a grob once.
  */
  if (!size)
    return;

  if (gi.origin_engraver ()->context () != context ())
    return;

  Real font_size
    = size + from_scm<double> (get_property (gi.grob (), "font-size"), 0);
  set_property (gi.grob (), "font-size", to_scm (font_size));
}

#include "translator.icc"

void
Font_size_engraver::boot ()
{
  ADD_ACKNOWLEDGER (font);
}

ADD_TRANSLATOR (Font_size_engraver,
                /* doc */
                R"(
Put @code{fontSize} into @code{font-size} grob property.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
fontSize
                )",

                /* write */
                R"(

                )");
