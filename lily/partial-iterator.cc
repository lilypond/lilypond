/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2011 Neil Puttock <n.puttock@gmail.com>

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
#include "input.hh"
#include "international.hh"
#include "moment.hh"
#include "music.hh"
#include "simple-music-iterator.hh"

class Partial_iterator : public Simple_music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
 protected:
  virtual void process (Moment);
};

void
Partial_iterator::process (Moment m)
{
  if (Duration *dur
      = unsmob_duration (get_music ()->get_property ("partial-duration")))
    {
      Context *ctx = get_outlet ();
      Moment now = ctx->now_mom ();
      if (now.main_part_ > Rational (0))
	get_music ()->origin ()->
	  warning (_ ("trying to use \\partial after the start of a piece"));
      Moment length = Moment (dur->get_length ());
      now = Moment (0, now.grace_part_);
      ctx->set_property ("measurePosition", (now - length).smobbed_copy ());
    }
  else
    programming_error ("invalid duration in \\partial");

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Partial_iterator);
