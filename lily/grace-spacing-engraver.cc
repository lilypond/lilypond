/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen <hanwen@lilypond.org>


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
#include "moment.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"

#include "translator.icc"

class Grace_spacing_engraver : public Engraver
{
  TRANSLATOR_DECLARATIONS (Grace_spacing_engraver);

protected:
  Moment last_moment_;
  Spanner *grace_spacing_;

  void process_music ();
  void stop_translation_timestep ();
};

Grace_spacing_engraver::Grace_spacing_engraver (Context *c)
  : Engraver (c)
{
  grace_spacing_ = 0;
}

void
Grace_spacing_engraver::process_music ()
{
  auto now = now_mom ();
  if (!last_moment_.grace_part_ and now.grace_part_)
    {
      grace_spacing_ = make_spanner ("GraceSpacing", SCM_EOL);
    }

  if (grace_spacing_ && (now.grace_part_ || last_moment_.grace_part_))
    {
      Grob *column = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      Pointer_group_interface::add_grob (grace_spacing_,
                                         ly_symbol2scm ("columns"), column);

      set_object (column, "grace-spacing", grace_spacing_->self_scm ());

      if (!grace_spacing_->get_bound (LEFT))
        grace_spacing_->set_bound (LEFT, column);
      else
        grace_spacing_->set_bound (RIGHT, column);
    }
}

void
Grace_spacing_engraver::stop_translation_timestep ()
{
  last_moment_ = now_mom ();

  if (!last_moment_.grace_part_)
    grace_spacing_ = 0;
}

void
Grace_spacing_engraver::boot ()
{
}

ADD_TRANSLATOR (Grace_spacing_engraver,
                R"(
Bookkeeping of shortest starting and playing notes in grace note runs.
                )",

                /* create */
                R"(
GraceSpacing
                )",

                /* read */
                R"(
currentMusicalColumn
                )",

                /* write */
                R"(

                )");
