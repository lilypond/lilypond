/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2011 Han-Wen Nienhuys

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

#include "column-x-positions.hh"
#include "spanner.hh"
#include "skyline.hh"

/*
  If you keep following offset reference points, you will always end
  up at the root object. This root object is called @ref{System}, and it
  represents a system (i.e. a line of music).
*/
class System : public Spanner
{
  int rank_;
  vector<Simple_spacer> simple_spacers_;
  Grob_array *all_elements_;
  void init_elements ();
  friend class Paper_score;     // ugh.
  Paper_score *pscore_; // ugh.

public:
  Paper_score *paper_score () const;
  Grob *get_vertical_alignment ();
  Grob *get_extremal_staff (Direction dir, Interval const &);
  Grob *get_pure_bound (Direction dir, int start, int end);
  Grob *get_maybe_pure_bound (Direction dir, bool pure, int start, int end);
  int get_rank () const;
  vector<Simple_spacer> get_simple_spacers (Real line_len, Real indent, bool ragged);
  void gen_simple_spacers (Real line_len, Real indent, bool ragged);
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
  DECLARE_SCHEME_CALLBACK (calc_pure_relevant_grobs, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (get_staves, (SCM));
  DECLARE_SCHEME_CALLBACK (get_spaceable_staves, (SCM));
  DECLARE_SCHEME_CALLBACK (get_nonspaceable_staves, (SCM));

  System (SCM);
  System (System const &);

  int element_count () const;
  int spanner_count () const;

  void break_into_pieces (vector<Column_x_positions> const &);
  DECLARE_GROB_INTERFACE ();

  vector<Item *> broken_col_range (Item const *, Item const *) const;
  vector<Grob *> used_columns () const;
  Paper_column *column (vsize i) const;

  void add_column (Paper_column *);
  void typeset_grob (Grob *);
  void pre_processing ();

  Interval begin_of_line_pure_height (vsize start, vsize end);
  Interval rest_of_line_pure_height (vsize start, vsize end);
  Interval pure_refpoint_extent (vsize start, vsize end);
  void collect_labels (Grob const *, SCM *);

protected:
  virtual void derived_mark () const;
  virtual Grob *clone () const;

private:
  Interval part_of_line_pure_height (vsize start, vsize end, bool begin);
};

void set_loose_columns (System *which, Column_x_positions const *posns);

#endif /* SYSTEM_HH */

