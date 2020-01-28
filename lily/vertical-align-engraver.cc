/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "context.hh"
#include "engraver.hh"
#include "grob-array.hh"
#include "international.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "spanner.hh"

#include "translator.icc"

using std::string;
using std::vector;

class Vertical_align_engraver : public Engraver
{
  Spanner *valign_;
  bool qualifies (Grob_info) const;
  SCM id_to_group_hashtab_;

public:
  TRANSLATOR_DECLARATIONS (Vertical_align_engraver);
  void acknowledge_axis_group (Grob_info);
  void acknowledge_outside_staff (Grob_info);

protected:
  void derived_mark () const override;
  void process_music ();
  void finalize () override;
  void initialize () override;

  bool top_level_;
};

void
Vertical_align_engraver::boot ()
{
  ADD_ACKNOWLEDGER (Vertical_align_engraver, axis_group);
  ADD_ACKNOWLEDGER (Vertical_align_engraver, outside_staff);
}

ADD_TRANSLATOR (Vertical_align_engraver,
                /* doc */
                "Catch groups (staves, lyrics lines, etc.) and stack them"
                " vertically.",

                /* create */
                "VerticalAlignment ",

                /* read */
                "alignAboveContext "
                "alignBelowContext "
                "hasAxisGroup ",

                /* write */
                "");

Vertical_align_engraver::Vertical_align_engraver (Context *c) : Engraver (c)
{
  valign_ = 0;
  id_to_group_hashtab_ = SCM_EOL;
  top_level_ = false;
}

void
Vertical_align_engraver::derived_mark () const
{
  scm_gc_mark (id_to_group_hashtab_);
}

void
Vertical_align_engraver::initialize ()
{
  id_to_group_hashtab_ = scm_c_make_hash_table (11);
}

void
Vertical_align_engraver::process_music ()
{
  if (!valign_ && !scm_is_null (id_to_group_hashtab_))
    {
      if (to_boolean (get_property ("hasAxisGroup")))
        {
          warning (_ ("Ignoring Vertical_align_engraver in VerticalAxisGroup"));
          id_to_group_hashtab_ = SCM_EOL;
          return;
        }

      top_level_ = to_boolean (get_property ("topLevelAlignment"));

      valign_ = make_spanner (top_level_ ? "VerticalAlignment" : "StaffGrouper",
                              SCM_EOL);
      valign_->set_bound (LEFT,
                          unsmob<Grob> (get_property ("currentCommandColumn")));
      Align_interface::set_ordered (valign_);
    }
}

void
Vertical_align_engraver::finalize ()
{
  if (valign_)
    {
      valign_->set_bound (RIGHT,
                          unsmob<Grob> (get_property ("currentCommandColumn")));
      valign_ = 0;
    }
}

bool
Vertical_align_engraver::qualifies (Grob_info i) const
{
  return has_interface<Axis_group_interface> (i.grob ())
         && !i.grob ()->get_parent (Y_AXIS)
         && !to_boolean (i.grob ()->get_property ("no-alignment"))
         && Axis_group_interface::has_axis (i.grob (), Y_AXIS);
}

void
Vertical_align_engraver::acknowledge_axis_group (Grob_info i)
{
  if (scm_is_null (id_to_group_hashtab_))
    return;

  if (top_level_ && qualifies (i))
    {
      string id = i.context ()->id_string ();

      scm_hash_set_x (id_to_group_hashtab_, ly_string2scm (id),
                      i.grob ()->self_scm ());

      SCM before_id = i.context ()->get_property ("alignAboveContext");
      SCM after_id = i.context ()->get_property ("alignBelowContext");

      SCM before = scm_hash_ref (id_to_group_hashtab_, before_id, SCM_BOOL_F);
      SCM after = scm_hash_ref (id_to_group_hashtab_, after_id, SCM_BOOL_F);

      Grob *before_grob = unsmob<Grob> (before);
      Grob *after_grob = unsmob<Grob> (after);

      Align_interface::add_element (valign_, i.grob ());

      if (before_grob || after_grob)
        {
          Grob_array *ga
              = unsmob<Grob_array> (valign_->get_object ("elements"));
          vector<Grob *> &arr = ga->array_reference ();

          Grob *added = arr.back ();
          arr.pop_back ();
          for (vsize i = 0; i < arr.size (); i++)
            {
              if (arr[i] == before_grob)
                {
                  arr.insert (arr.begin () + i, added);

                  /* Only set staff affinity if it already has one.  That way we
                     won't
                     set staff-affinity on things that don't want it (like
                     staves). */
                  if (scm_is_number (added->get_property ("staff-affinity")))
                    added->set_property ("staff-affinity", scm_from_int (DOWN));
                  break;
                }
              else if (arr[i] == after_grob)
                {
                  arr.insert (arr.begin () + i + 1, added);
                  if (scm_is_number (added->get_property ("staff-affinity")))
                    added->set_property ("staff-affinity", scm_from_int (UP));
                  break;
                }
            }
        }
    }
  else if (qualifies (i))
    {
      Pointer_group_interface::add_grob (valign_, ly_symbol2scm ("elements"),
                                         i.grob ());
      if (!unsmob<Grob> (i.grob ()->get_object ("staff-grouper")))
        i.grob ()->set_object ("staff-grouper", valign_->self_scm ());
    }
}

void
Vertical_align_engraver::acknowledge_outside_staff (Grob_info i)
{
  if (!top_level_) // valign_ is a staff grouper
    {
      if (valign_)
        {
          // Claim outside-staff grobs created by engravers in this immediate
          // context.
          if (i.context () == context ())
            i.grob ()->set_parent (valign_, Y_AXIS);
        }
      else
        {
          programming_error (
              "cannot claim outside-staff grob before creating staff grouper");
        }
    }
}
