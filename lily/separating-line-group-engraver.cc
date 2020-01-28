/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "accidental-placement.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "grob-array.hh"
#include "note-spacing.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "separation-item.hh"
#include "spanner.hh"

#include "translator.icc"

using std::vector;

struct Spacings
{
  Item *staff_spacing_;
  vector<Item *> note_spacings_;

  Spacings () { staff_spacing_ = 0; }

  bool is_empty () const { return !staff_spacing_ && !note_spacings_.size (); }
  void clear ()
  {
    staff_spacing_ = 0;
    note_spacings_.clear ();
  }
};

class Separating_line_group_engraver : public Engraver
{
protected:
  Spacings current_spacings_;
  Spacings last_spacings_;

  void acknowledge_item (Grob_info);
  void acknowledge_break_aligned (Grob_info);
  void stop_translation_timestep ();
  void start_translation_timestep ();

  vector<Grob *> break_aligned_;

public:
  TRANSLATOR_DECLARATIONS (Separating_line_group_engraver);
};

Separating_line_group_engraver::Separating_line_group_engraver (Context *c)
    : Engraver (c)
{
}

void
Separating_line_group_engraver::acknowledge_item (Grob_info i)
{
  Item *it = dynamic_cast<Item *> (i.grob ());

  if (has_interface<Note_spacing> (it))
    {
      current_spacings_.note_spacings_.push_back (it);
      return;
    }

  if (Item::is_non_musical (it) && !current_spacings_.staff_spacing_
      && to_boolean (get_property ("createSpacing")))
    {
      Grob *col = unsmob<Grob> (get_property ("currentCommandColumn"));

      current_spacings_.staff_spacing_ = make_item ("StaffSpacing", SCM_EOL);
      context ()->set_property ("hasStaffSpacing", SCM_BOOL_T);

      Pointer_group_interface::add_grob (current_spacings_.staff_spacing_,
                                         ly_symbol2scm ("left-items"), col);

      if (!last_spacings_.note_spacings_.size ()
          && last_spacings_.staff_spacing_)
        {
          SCM ri = last_spacings_.staff_spacing_->get_object ("right-items");
          Grob_array *ga = unsmob<Grob_array> (ri);
          if (!ga)
            {
              SCM ga_scm = Grob_array::make_array ();
              last_spacings_.staff_spacing_->set_object ("right-items", ga_scm);
              ga = unsmob<Grob_array> (ga_scm);
            }

          ga->clear ();
          ga->add (col);
        }
    }
}

void
Separating_line_group_engraver::acknowledge_break_aligned (Grob_info gi)
{
  break_aligned_.push_back (gi.grob ());
}

void
Separating_line_group_engraver::start_translation_timestep ()
{
  context ()->unset_property (ly_symbol2scm ("hasStaffSpacing"));
}

void
Separating_line_group_engraver::stop_translation_timestep ()
{
  for (vsize i = 0; i < break_aligned_.size (); i++)
    {
      SCM smob = break_aligned_[i]->self_scm ();

      if (Item *sp = current_spacings_.staff_spacing_)
        Pointer_group_interface::add_grob (
            sp, ly_symbol2scm ("left-break-aligned"), smob);

      for (vsize j = 0; j < last_spacings_.note_spacings_.size (); j++)
        Pointer_group_interface::add_grob (
            last_spacings_.note_spacings_[j],
            ly_symbol2scm ("right-break-aligned"), smob);
    }

  if (!current_spacings_.is_empty ())
    last_spacings_ = current_spacings_;

  if (Item *sp = current_spacings_.staff_spacing_)
    if (Grob *col = unsmob<Grob> (get_property ("currentMusicalColumn")))
      Pointer_group_interface::add_grob (sp, ly_symbol2scm ("right-items"),
                                         col);

  current_spacings_.clear ();
  break_aligned_.clear ();
}

void
Separating_line_group_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Separating_line_group_engraver, item);
  ADD_ACKNOWLEDGER (Separating_line_group_engraver, break_aligned);
}

ADD_TRANSLATOR (Separating_line_group_engraver,
                /* doc */
                "Generate objects for computing spacing parameters.",

                /* create */
                "StaffSpacing ",

                /* read */
                "createSpacing ",

                /* write */
                "hasStaffSpacing ");
