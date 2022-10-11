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

#include "axis-group-interface.hh"
#include "dot-column.hh"
#include "engraver.hh"
#include "item.hh"
#include "note-column.hh"
#include "pointer-group-interface.hh"
#include "rhythmic-head.hh"
#include "stem.hh"

#include "translator.icc"

using std::vector;

/*
  this engraver  glues together stems, rests and note heads into a NoteColumn
  grob.

  It also generates spacing objects.  Originally, we have tried to
  have the spacing functionality at different levels.

  - by simply using the sequence of Separation-item as
  spacing-sequences (at staff level). Unfortunately, this fucks up if
  there are different kinds of tuplets in different voices (8th and
  8ths triplets combined made the program believe there were 1/12 th
  notes.).

  Doing it in a separate engraver using timing info is generally
  complicated (start/end time management), and fucks up if a voice
  changes staff.

  Now we do it from here again. This has the problem that voices can
  appear and disappear at will, leaving lots of loose ends (the note
  spacing engraver don't know where to connect the last note of the
  voice on the right with), but we don't complain about those, and let
  the default spacing do its work.
*/

class Rhythmic_column_engraver : public Engraver
{
  vector<Grob *> rheads_;
  Grob *stem_;
  Grob *flag_;
  Grob *note_column_;

  TRANSLATOR_DECLARATIONS (Rhythmic_column_engraver);

protected:
  void acknowledge_stem (Grob_info);
  void acknowledge_flag (Grob_info);
  void acknowledge_rhythmic_head (Grob_info);
  void process_acknowledged ();
  void stop_translation_timestep ();
};

Rhythmic_column_engraver::Rhythmic_column_engraver (Context *c)
  : Engraver (c)
{

  stem_ = 0;
  flag_ = 0;
  note_column_ = 0;
}

void
Rhythmic_column_engraver::process_acknowledged ()
{
  if (rheads_.size ())
    {
      if (!note_column_)
        note_column_ = make_item ("NoteColumn", rheads_[0]->self_scm ());

      for (vsize i = 0; i < rheads_.size (); i++)
        if (!rheads_[i]->get_x_parent ())
          Note_column::add_head (note_column_, rheads_[i]);

      rheads_.resize (0);
    }

  if (note_column_)
    {
      if (stem_ && !stem_->get_x_parent ())
        {
          Note_column::set_stem (note_column_, stem_);
          stem_ = 0;
        }

      if (flag_)
        {
          Axis_group_interface::add_element (note_column_, flag_);
          flag_ = 0;
        }
    }
}

void
Rhythmic_column_engraver::acknowledge_stem (Grob_info i)
{
  stem_ = i.grob ();
}

void
Rhythmic_column_engraver::acknowledge_flag (Grob_info i)
{
  flag_ = i.grob ();
}

void
Rhythmic_column_engraver::acknowledge_rhythmic_head (Grob_info i)
{
  rheads_.push_back (i.grob ());
}

void
Rhythmic_column_engraver::stop_translation_timestep ()
{
  note_column_ = 0;
  stem_ = 0;
  flag_ = 0;
}

void
Rhythmic_column_engraver::boot ()
{
  ADD_ACKNOWLEDGER (stem);
  ADD_ACKNOWLEDGER (flag);
  ADD_ACKNOWLEDGER (rhythmic_head);
}

ADD_TRANSLATOR (Rhythmic_column_engraver,
                /* doc */
                R"(
Generate @code{NoteColumn}, an object that groups stems, note heads, and rests.
                )",

                /* create */
                R"(
NoteColumn
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
