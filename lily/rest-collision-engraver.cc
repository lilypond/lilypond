/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <set>

#include "duration.hh"
#include "engraver.hh"
#include "international.hh"
#include "item.hh"
#include "moment.hh"
#include "note-column.hh"
#include "paper-column.hh"
#include "rest.hh"
#include "rest-collision.hh"
#include "rhythmic-head.hh"
#include "stream-event.hh"
#include "warn.hh"

class Rest_collision_engraver : public Engraver
{
protected:
  Grob *rest_collision_;

  void process_acknowledged ();
  void stop_translation_timestep ();
public:
  TRANSLATOR_DECLARATIONS (Rest_collision_engraver);
};

Rest_collision_engraver::Rest_collision_engraver ()
{
  rest_collision_ = 0;
}

void
Rest_collision_engraver::process_acknowledged ()
{
  vsize rest_count = 0;
  set<Grob*> columns;
  Moment now = now_mom ();

  for (SCM s = get_property ("busyGrobs"); scm_is_pair (s); s = scm_cdr (s))
    {
      Grob *g = unsmob_grob (scm_cdar (s));
      Moment *m = unsmob_moment (scm_caar (s));
      if (!g || !m)
	continue;

      if (Rhythmic_head::has_interface (g) && (*m) > now)
	{
	  Grob *column = g->get_parent (X_AXIS);
	  if (!column)
	    {
	      g->warning (_ ("rhythmic head is not part of a rhythmic column"));
	      continue;
	    }

	  // Only include rests that start now. Include notes that started any time.
	  Paper_column *paper_column = dynamic_cast<Item*> (column)->get_column ();
	  if (!Rest::has_interface (g) || !paper_column || Paper_column::when_mom (paper_column) == now)
	    {
	      columns.insert (column);
	      rest_count += Note_column::has_rests (column);
	    }
	}
    }

  if (!rest_collision_ && rest_count && columns.size () > 1)
    {
      rest_collision_ = make_item ("RestCollision", SCM_EOL);
      for (set<Grob*>::iterator i = columns.begin (); i != columns.end (); ++i)
	Rest_collision::add_column (rest_collision_, *i);
    }
}

void
Rest_collision_engraver::stop_translation_timestep ()
{
  rest_collision_ = 0;
}

#include "translator.icc"

ADD_TRANSLATOR (Rest_collision_engraver,
		/* doc */
		"Handle collisions of rests.",

		/* create */
		"RestCollision ",

		/* read */
		"busyGrobs ",

		/* write */
		""
		);
