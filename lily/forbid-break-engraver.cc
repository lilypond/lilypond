/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--_2005 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "duration.hh"
#include "engraver.hh"
#include "grob.hh"
#include "input.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"

#include "translator.icc"

class Forbid_line_break_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Forbid_line_break_engraver);
  void start_translation_timestep ();
};

Forbid_line_break_engraver::Forbid_line_break_engraver ()
{
}

void
Forbid_line_break_engraver::start_translation_timestep ()
{
  /*
    Check for running note heads. This should probably be done elsewhere.
  */
  SCM busy = get_property ("busyGrobs");

  Moment now = now_mom ();
  while (scm_is_pair (busy) && unsmob_moment (scm_caar (busy))->main_part_ == now.main_part_)
    busy = scm_cdr (busy);

  while (scm_is_pair (busy))
    {
      Grob *g = unsmob_grob (scm_cdar (busy));
      if (g->internal_has_interface (ly_symbol2scm ("rhythmic-grob-interface")))
        context ()->get_score_context ()->set_property ("forbidBreak", SCM_BOOL_T);
      busy = scm_cdr (busy);
    }
}

ADD_TRANSLATOR (Forbid_line_break_engraver,
		/* doc */
		"Forbid line breaks when note heads are still playing at some"
		" point.",

		/* create */
		"",

		/* read */
		"busyGrobs ",

		/* write */
		"forbidBreak "
		);
