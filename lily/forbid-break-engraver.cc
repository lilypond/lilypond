/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
  void pre_process_music ();
};

Forbid_line_break_engraver::Forbid_line_break_engraver (Context *c)
  : Engraver (c)
{
}

void
Forbid_line_break_engraver::pre_process_music ()
{
  /*
    Check for running note heads. This should probably be done elsewhere.
  */
  SCM busy = get_property (this, "busyGrobs");

  auto now = now_mom ();
  while (scm_is_pair (busy)
         && unsmob<Moment> (scm_caar (busy))->main_part_ == now.main_part_)
    busy = scm_cdr (busy);

  while (scm_is_pair (busy))
    {
      Grob *g = unsmob<Grob> (scm_cdar (busy));
      if (g->internal_has_interface (ly_symbol2scm ("rhythmic-grob-interface")))
        set_property (find_score_context (), "forbidBreak", SCM_BOOL_T);
      busy = scm_cdr (busy);
    }
}

void
Forbid_line_break_engraver::boot ()
{
}

ADD_TRANSLATOR (Forbid_line_break_engraver,
                /* doc */
                R"(
Forbid line breaks when note heads are still playing at some point.
                )",

                /* create */
                R"(

                )",

                /* read */
                R"(
busyGrobs
                )",

                /* write */
                R"(
forbidBreak
                )");
