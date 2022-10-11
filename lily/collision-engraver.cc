/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "note-column.hh"
#include "note-collision.hh"
#include "axis-group-interface.hh"
#include "item.hh"

#include "translator.icc"

using std::vector;

class Collision_engraver : public Engraver
{
  Item *col_;
  vector<Item *> note_columns_;

protected:
  void acknowledge_note_column (Grob_info_t<Item>);
  void process_acknowledged ();
  void stop_translation_timestep ();

public:
  TRANSLATOR_DECLARATIONS (Collision_engraver);
};

void
Collision_engraver::process_acknowledged ()
{
  if (col_ || note_columns_.size () < 2)
    return;
  if (!col_)
    col_ = make_item ("NoteCollision", SCM_EOL);

  for (vsize i = 0; i < note_columns_.size (); i++)
    Note_collision_interface::add_column (col_, note_columns_[i]);
}

void
Collision_engraver::acknowledge_note_column (Grob_info_t<Item> info)
{
  auto *const item = info.grob ();

  /*should check Y axis? */
  if (Note_column::has_rests (item) || item->get_x_parent ())
    return;

  if (from_scm<bool> (get_property (item, "ignore-collision")))
    return;

  note_columns_.push_back (item);
}

void
Collision_engraver::stop_translation_timestep ()
{
  col_ = 0;
  note_columns_.clear ();
}

Collision_engraver::Collision_engraver (Context *c)
  : Engraver (c)
{
  col_ = 0;
}

void
Collision_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_column);
}

ADD_TRANSLATOR (Collision_engraver,
                /* doc */
                R"(
Collect @code{NoteColumns}, and as soon as there are two or more, put them in a
@code{NoteCollision} object.
                )",

                /* create */
                R"(
NoteCollision
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
