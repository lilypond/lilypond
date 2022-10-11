/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010--2022 Carl Sorensen <c_sorensen@byu.edu>

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

#include "articulations.hh"
#include "ly-smob-list.hh"
#include "stream-event.hh"
#include "warn.hh"
#include "context.hh"

using std::vector;

/*
  Return an articulation list given a note_events vector and an
  articulation_events vector.

  This is necessary, because the articulations come as events if
  they are entered outside of a chord structure, and as articulations
  if they are inside the chord structure.  So potentially we need to
  combine the two types.
*/

SCM
articulation_list (vector<Stream_event *> note_events,
                   vector<Stream_event *> articulation_events,
                   char const *articulation_name)
{
  SCM articulations = SCM_EOL;
  vsize j = 0;

  for (auto *event : note_events)
    {
      Stream_event *articulation_event = nullptr;

      /*
        For notes inside a chord construct, string indications are
        stored as articulations on the note, so we check through
        the notes
      */
      for (auto *art :
           ly_smob_list<Stream_event> (get_property (event, "articulations")))
        {
          if (art && art->in_event_class (articulation_name))
            {
              articulation_event = art;
              break;
            }
        }

      /*
        For string indications listed outside a chord construct,
        a string_number_event is generated, so if there was no string
        in the articulations, we check for string events outside
        the chord construct
      */
      if (!articulation_event && j < articulation_events.size ())
        {
          articulation_event = articulation_events[j];
          if (j + 1 < articulation_events.size ())
            j++;
        }
      articulations = scm_cons (
        (articulation_event ? articulation_event->self_scm () : SCM_EOL),
        articulations);
    }

  return scm_reverse_x (articulations, SCM_EOL);
}
