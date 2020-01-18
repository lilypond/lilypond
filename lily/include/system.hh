/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2019 Han-Wen Nienhuys

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

#ifndef SYSTEM_HH
#define SYSTEM_HH

#include <cstdint>
#include <limits>

#include "column-x-positions.hh"
#include "paper-column.hh"
#include "spanner.hh"
#include "skyline.hh"

/*
  If you keep following offset reference points, you will always end
  up at the root object. This root object is called @ref{System}, and it
  represents a system (i.e. a line of music).
*/
class System : public Spanner
{
public:
  // See Substitution_entry.
  typedef int16_t rank_type;

private:
  rank_type rank_;
  Grob_array *all_elements_;
  void init_elements ();
  friend class Paper_score;     // ugh.
  Paper_score *pscore_; // ugh.

public:
  Paper_score *paper_score () const;
  Grob *get_neighboring_staff (Direction dir, Grob *vertical_axis_group, Interval_t<int> bounds);
  bool accepts_as_bound_item (const Item *) const override;
  bool accepts_as_bound_paper_column (const Paper_column *) const override;
  Paper_column *get_bound (Direction d) const {
    // This is safe because only Paper_columns are accepted as bounds.
    return static_cast<Paper_column *> (Spanner::get_bound (d));
  }
  Paper_column *get_pure_bound (Direction dir, vsize start, vsize end);
  Paper_column *get_maybe_pure_bound (Direction dir, bool pure,
                                      vsize start, vsize end);
  rank_type get_rank () const { return rank_; }
  vector<Real> get_footnote_heights_in_range (vsize st, vsize end);
  vector<Real> get_in_note_heights_in_range (vsize st, vsize end);
  vector<Real> internal_get_note_heights_in_range (vsize st, vsize end, bool foot);
  vector<Grob *> get_footnote_grobs_in_range (vsize st, vsize end);
  vsize num_footnotes ();
  void do_break_substitution_and_fixup_refpoints ();
  void post_processing ();
  SCM get_paper_system ();
  SCM get_paper_systems ();
  SCM get_broken_system_grobs ();
  SCM get_broken_footnote_stencils ();

  DECLARE_SCHEME_CALLBACK (footnotes_before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (footnotes_after_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (vertical_skyline_elements, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_pure_relevant_grobs, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (get_staves, (SCM));
  DECLARE_SCHEME_CALLBACK (get_spaceable_staves, (SCM));
  DECLARE_SCHEME_CALLBACK (get_nonspaceable_staves, (SCM));
  DECLARE_SCHEME_CALLBACK (get_vertical_alignment, (SCM));

  System (SCM);
  System (System const &);
  System *original () const
  {
    // safe: if there is an original, it is because this was cloned from it
    return static_cast<System *> (Spanner::original ());
  }

  vsize element_count () const;
  vsize spanner_count () const;

  void break_into_pieces (vector<Column_x_positions> const &);

  vector<Item *> broken_col_range (Item const *, Item const *) const;
  vector<Paper_column *> used_columns_in_range (vsize start, vsize end) const;
  vector<Paper_column *> used_columns () const
  {
    return used_columns_in_range(0, std::numeric_limits<vsize>::max ());
  }
  Paper_column *column (vsize i) const;

  void add_column (Paper_column *);
  void typeset_grob (Grob *);
  void pre_processing ();

  Interval begin_of_line_pure_height (vsize start, vsize end);
  Interval rest_of_line_pure_height (vsize start, vsize end);
  Interval pure_refpoint_extent (vsize start, vsize end);
  void collect_labels (Grob const *, SCM *);

protected:
  void derived_mark () const override;
  System *clone () const override { return new System (*this); }

private:
  Interval part_of_line_pure_height (vsize start, vsize end, bool begin);
};

void set_loose_columns (System *which, Column_x_positions const *posns);

#endif /* SYSTEM_HH */

