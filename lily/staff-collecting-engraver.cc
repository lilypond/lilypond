/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2001--2020  Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "grob.hh"
#include "staff-symbol.hh"

class Staff_collecting_engraver : public Engraver
{
public:
  TRANSLATOR_DECLARATIONS (Staff_collecting_engraver);
  void acknowledge_staff_symbol (Grob_info);
  void acknowledge_end_staff_symbol (Grob_info);
};

Staff_collecting_engraver::Staff_collecting_engraver (Context *c) : Engraver (c)
{
}

void
Staff_collecting_engraver::acknowledge_staff_symbol (Grob_info gi)
{
  SCM staffs = get_property ("stavesFound");
  staffs = scm_cons (gi.grob ()->self_scm (), staffs);

  context ()->set_property ("stavesFound", staffs);
}

void
Staff_collecting_engraver::acknowledge_end_staff_symbol (Grob_info gi)
{
  SCM staffs = get_property ("stavesFound");
  staffs = scm_delq (gi.grob ()->self_scm (), staffs);

  context ()->set_property ("stavesFound", staffs);
}

#include "translator.icc"

void
Staff_collecting_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Staff_collecting_engraver, staff_symbol);
  ADD_END_ACKNOWLEDGER (Staff_collecting_engraver, staff_symbol);
}

ADD_TRANSLATOR (Staff_collecting_engraver,
                /* doc */
                "Maintain the @code{stavesFound} variable.",

                /* create */
                "",

                /* read */
                "stavesFound ",

                /* write */
                "stavesFound ");
