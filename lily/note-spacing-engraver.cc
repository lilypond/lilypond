/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>

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

#include "grob-array.hh"
#include "context.hh"
#include "item.hh"
#include "pointer-group-interface.hh"

#include <map>

#include "translator.icc"

using std::map;

class Note_spacing_engraver : public Engraver
{
  typedef map<Context *, Grob *> Last_spacing_map;
  Last_spacing_map last_spacings_;
  Grob *last_spacing_;

  Grob *spacing_;

  void add_spacing_item (Grob *);
  TRANSLATOR_DECLARATIONS (Note_spacing_engraver);

protected:
  void acknowledge_rhythmic_grob (Grob_info);
  void acknowledge_note_column (Grob_info_t<Item>);
  void stop_translation_timestep ();
  void finalize () override;
  void derived_mark () const override;
};

void
Note_spacing_engraver::derived_mark () const
{
  for (Last_spacing_map::const_iterator i = last_spacings_.begin ();
       i != last_spacings_.end (); i++)
    scm_gc_mark (i->first->self_scm ());
}

Note_spacing_engraver::Note_spacing_engraver (Context *c)
  : Engraver (c)
{
  spacing_ = 0;
  last_spacing_ = 0;
}

void
Note_spacing_engraver::add_spacing_item (Grob *g)
{
  if (!spacing_)
    {
      spacing_ = make_item ("NoteSpacing", g->self_scm ());
    }

  if (spacing_)
    {
      Pointer_group_interface::add_grob (spacing_, ly_symbol2scm ("left-items"),
                                         g);

      if (last_spacing_)
        Pointer_group_interface::add_grob (last_spacing_,
                                           ly_symbol2scm ("right-items"), g);
    }
}

void
Note_spacing_engraver::acknowledge_note_column (Grob_info_t<Item> gi)
{
  add_spacing_item (gi.grob ());
}

void
Note_spacing_engraver::acknowledge_rhythmic_grob (Grob_info gi)
{
  add_spacing_item (gi.grob ());
}

void
Note_spacing_engraver::finalize ()
{
  Context *parent = context ()->get_parent ();
  Grob *last_spacing = last_spacings_[parent];

  if (last_spacing
      && !unsmob<Grob_array> (get_object (last_spacing, "right-items")))
    {
      Grob *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));

      Pointer_group_interface::add_grob (last_spacing,
                                         ly_symbol2scm ("right-items"), col);
    }
}

void
Note_spacing_engraver::stop_translation_timestep ()
{
  Context *parent = context ()->get_parent ();
  Grob *last_spacing = last_spacings_[parent];

  if (last_spacing && from_scm<bool> (get_property (this, "hasStaffSpacing")))
    {
      Grob *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      Pointer_group_interface::add_grob (last_spacing,
                                         ly_symbol2scm ("right-items"), col);
    }

  if (spacing_)
    {
      last_spacings_[parent] = spacing_;
      last_spacing_ = spacing_;
      spacing_ = 0;
    }
}

void
Note_spacing_engraver::boot ()
{
  ADD_ACKNOWLEDGER (note_column);
  ADD_ACKNOWLEDGER (rhythmic_grob);
}

ADD_TRANSLATOR (Note_spacing_engraver,
                /* doc */
                R"(
Generate @code{NoteSpacing}, an object linking horizontal lines for use in
spacing.
                )",

                /* create */
                R"(
NoteSpacing
                )",

                /* read */
                R"(

                )",

                /* write */
                R"(

                )");
