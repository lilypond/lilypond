/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2010 Carl D. Sorensen

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

#include <cctype>
#include <cstdio>

#include "engraver.hh"

using namespace std;

#include "context.hh"
#include "item.hh"

#include "translator.icc"

/*
   Change tab-note-head properties for a note_head at the right end of a tie
*/
class Tab_tie_follow_engraver : public Engraver
{
  vector<Grob*> ties_;
  vector<Grob*> note_heads_;

public:
  TRANSLATOR_DECLARATIONS (Tab_tie_follow_engraver);

protected:
  DECLARE_ACKNOWLEDGER (tie);
  DECLARE_ACKNOWLEDGER (tab_note_head);
  void process_acknowledged ();

  void stop_translation_timestep ();
};

Tab_tie_follow_engraver::Tab_tie_follow_engraver ()
{
}

void
Tab_tie_follow_engraver::acknowledge_tie (Grob_info info)
{
   ties_.push_back (info.grob ());
}

void
Tab_tie_follow_engraver::acknowledge_tab_note_head (Grob_info info)
{
  note_heads_.push_back (info.grob ());
}

void
Tab_tie_follow_engraver::process_acknowledged ()
{
  if (ties_.size () && note_heads_.size ())
    {
      SCM proc = ly_lily_module_constant ("ly:spanner-bound");
      for (vsize i = 0; i < ties_.size (); i++)
        {
          SCM right_bound = scm_call_2 (proc,
                                        ties_[i]->self_scm (),
                                        scm_from_int (RIGHT));

          for (vsize k = 0; k < note_heads_.size (); k++)
            if (right_bound == note_heads_[k]->self_scm ())
              note_heads_[k]->set_property ("tie-follow", SCM_BOOL_T);
         }
    }
}

void
Tab_tie_follow_engraver::stop_translation_timestep ()
{
  ties_.clear ();
  note_heads_.clear();
}

ADD_ACKNOWLEDGER (Tab_tie_follow_engraver, tie);
ADD_ACKNOWLEDGER (Tab_tie_follow_engraver, tab_note_head);

ADD_TRANSLATOR (Tab_tie_follow_engraver,
		/* doc */
		"Adjust TabNoteHead properties when a tie is followed"
		" by a slur or glissando.",

		/* create */
		" ",

		/* read */
                " ",

		/* write */
                " "
                );

