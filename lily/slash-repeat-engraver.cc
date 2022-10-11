/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2000--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>, Erik Sandberg
  <mandolaerik@gmail.com>

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

#include "item.hh"
#include "engraver.hh"
#include "stream-event.hh"

#include "translator.icc"

/*
  This acknowledges repeated music with "percent" style.  It typesets
  a slash sign or double percent sign.
*/
class Slash_repeat_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Slash_repeat_engraver);

protected:
  Stream_event *slash_;

protected:
  void listen_repeat_slash (Stream_event *);
  void process_music ();
};

Slash_repeat_engraver::Slash_repeat_engraver (Context *c)
  : Engraver (c)
{
  slash_ = 0;
}

void
Slash_repeat_engraver::listen_repeat_slash (Stream_event *ev)
{
  assign_event_once (slash_, ev);
}

void
Slash_repeat_engraver::process_music ()
{
  if (slash_)
    {
      SCM count = get_property (slash_, "slash-count");
      if (from_scm<int> (count) == 0)
        make_item ("DoubleRepeatSlash", slash_->self_scm ());
      else
        make_item ("RepeatSlash", slash_->self_scm ());
      slash_ = 0;
    }
}

void
Slash_repeat_engraver::boot ()
{
  ADD_LISTENER (repeat_slash);
}

ADD_TRANSLATOR (Slash_repeat_engraver,
                /* doc */
                R"(
Make beat repeats.
                )",

                /* create */
                R"(
DoubleRepeatSlash
RepeatSlash
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
