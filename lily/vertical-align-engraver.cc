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

#include "context.hh"
#include "paper-column.hh"
#include "align-interface.hh"
#include "axis-group-interface.hh"
#include "engraver.hh"
#include "international.hh"
#include "spanner.hh"
#include "pointer-group-interface.hh"
#include "grob-array.hh"

#include "translator.icc"

using std::string;
using std::vector;

class Vertical_align_engraver : public Engraver
{
  Spanner *valign_;
  SCM id_to_group_hashtab_;

public:
  TRANSLATOR_DECLARATIONS (Vertical_align_engraver);
  void acknowledge_hara_kiri_group_spanner (Grob_info);
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
  ADD_ACKNOWLEDGER (hara_kiri_group_spanner);
  ADD_ACKNOWLEDGER (outside_staff);
}

ADD_TRANSLATOR (Vertical_align_engraver,
                /* doc */
                R"(
Catch groups (staves, lyrics lines, etc.) and stack them vertically.
                )",

                /* create */
                R"(
StaffGrouper
VerticalAlignment
                )",

                /* read */
                R"(
alignAboveContext
alignBelowContext
hasAxisGroup
                )",

                /* write */
                R"(

                )");

// TODO: consider splitting out a Staff_grouper_engraver.
// The code paths for top_level_ being true or false seem
// to share very little. --JeanAS

Vertical_align_engraver::Vertical_align_engraver (Context *c)
  : Engraver (c)
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
      if (from_scm<bool> (get_property (this, "hasAxisGroup")))
        {
          warning (_ ("Ignoring Vertical_align_engraver in VerticalAxisGroup"));
          id_to_group_hashtab_ = SCM_EOL;
          return;
        }

      top_level_ = from_scm<bool> (get_property (this, "topLevelAlignment"));

      valign_ = make_spanner (top_level_ ? "VerticalAlignment" : "StaffGrouper",
                              SCM_EOL);
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      valign_->set_bound (LEFT, col);
    }
}

void
Vertical_align_engraver::finalize ()
{
  if (valign_)
    {
      auto *col = unsmob<Grob> (get_property (this, "currentCommandColumn"));
      valign_->set_bound (RIGHT, col);
      valign_ = 0;
    }
}

void
Vertical_align_engraver::acknowledge_hara_kiri_group_spanner (Grob_info i)
{
  if (scm_is_null (id_to_group_hashtab_))
    return;

  if (top_level_)
    {
      auto *const origin_ctx = i.origin_engraver ()->context ();
      const auto &id = origin_ctx->id_string ();

      scm_hash_set_x (id_to_group_hashtab_, ly_string2scm (id),
                      i.grob ()->self_scm ());

      SCM before_id = get_property (origin_ctx, "alignAboveContext");
      SCM after_id = get_property (origin_ctx, "alignBelowContext");

      Align_interface::add_element (valign_, i.grob ());

      if (scm_is_null (before_id) && scm_is_null (after_id))
        return;

      SCM before = scm_hash_ref (id_to_group_hashtab_, before_id, SCM_BOOL_F);
      SCM after = scm_hash_ref (id_to_group_hashtab_, after_id, SCM_BOOL_F);

      if (scm_is_false (before) && scm_is_false (after))
        {
          if (scm_is_string (before_id))
            {
              warning (_f ("alignAboveContext not found: %s",
                           ly_scm2string (before_id)));
            }
          else
            {
              warning (_f ("alignBelowContext not found: %s",
                           ly_scm2string (after_id)));
            }
          return;
        }

      Grob *before_grob = unsmob<Grob> (before);
      Grob *after_grob = unsmob<Grob> (after);

      Grob_array *ga = unsmob<Grob_array> (get_object (valign_, "elements"));
      vector<Grob *> &arr = ga->array_reference ();

      Grob *added = arr.back ();
      arr.pop_back ();
      for (vsize i = 0; i < arr.size (); i++)
        {
          if (arr[i] == before_grob)
            {
              arr.insert (arr.begin () + i, added);

              /* Only set staff affinity if it already has one.  That way we won't
                 set staff-affinity on things that don't want it (like staves). */
              if (scm_is_number (get_property (added, "staff-affinity")))
                set_property (added, "staff-affinity", to_scm (DOWN));
              break;
            }
          else if (arr[i] == after_grob)
            {
              arr.insert (arr.begin () + i + 1, added);
              if (scm_is_number (get_property (added, "staff-affinity")))
                set_property (added, "staff-affinity", to_scm (UP));
              break;
            }
        }
    }
  else
    {
      Pointer_group_interface::add_grob (valign_, ly_symbol2scm ("elements"),
                                         i.grob ());
      if (!unsmob<Grob> (get_object (i.grob (), "staff-grouper")))
        set_object (i.grob (), "staff-grouper", valign_->self_scm ());
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
          if (i.origin_engraver ()->context () == context ())
            i.grob ()->set_y_parent (valign_);
        }
      else
        {
          programming_error (
            "cannot claim outside-staff grob before creating staff grouper");
        }
    }
}
