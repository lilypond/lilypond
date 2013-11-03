/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2012 Neil Puttock <n.puttock@gmail.com>

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
  if (Duration * dur
      = unsmob_duration (get_music ()->get_property ("duration")))
    {
      // Partial_iterator is an iterator rather than an engraver, so
      // the active context it is getting called in does not depend on
      // which context definition the engraver might be defined.
      //
      // Using where_defined to find the context where measurePosition
      // should be overwritten does not actually work since the
      // Timing_translator does not set measurePosition when
      // initializing.

      Context *timing = unsmob_context (scm_call_2 (ly_lily_module_constant ("ly:context-find"),
                                                    get_outlet ()->self_scm (),
                                                    ly_symbol2scm ("Timing")));

      if (!timing)
        programming_error ("missing Timing in \\partial");
      else
        {
          Moment mp = robust_scm2moment (timing->get_property ("measurePosition"),
                                         Rational (0));

          if (mp.main_part_ > Rational (0))
            mp.main_part_ = measure_length (timing);
          else
            mp.main_part_ = 0;

          Moment length = Moment (dur->get_length ());
          timing->set_property ("measurePosition", (mp - length).smobbed_copy ());
        }
    }
  else
    programming_error ("invalid duration in \\partial");

  Simple_music_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Partial_iterator);
