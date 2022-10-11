/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

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

#ifndef SPANNER_HH
#define SPANNER_HH

#include "grob.hh"
#include "rod.hh"

#include <vector>

class Engraver;

/** A symbol which is attached between two columns. A spanner is a
    symbol which spans across several columns, so its final appearance
    can only be calculated after the breaking problem is solved.

    Examples

    * (de)crescendo
    * slur
    * beam
    * bracket

    Spanner should know about the items which it should consider:
    e.g. slurs should be steep enough to "enclose" all those items. This
    is absolutely necessary for beams, since they have to adjust the
    length of stems of notes they encompass.
*/

class Preinit_Spanner
{
protected:
  Drul_array<Item *> spanned_drul_ {nullptr, nullptr};
  SCM pure_property_cache_ = SCM_UNDEFINED;

public:
  std::vector<Spanner *> broken_intos_;
};

class Spanner : public Preinit_Spanner, public Grob
{
  vsize break_index_;

public:
  OVERRIDE_CLASS_NAME (Spanner);
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_normalized_endpoints, (SCM));
  DECLARE_SCHEME_CALLBACK (bounds_width, (SCM));
  DECLARE_SCHEME_CALLBACK (kill_zero_spanned_time, (SCM));

  vsize get_break_index () const;
  Spanner *broken_neighbor (Direction d) const;

  // todo: move to somewhere else.
  Real get_broken_left_end_align () const;
  void substitute_one_mutable_property (SCM sym, SCM val);

  Interval_t<Moment> spanned_time () const;
  Interval_t<int> spanned_column_rank_interval () const override;
  System_rank_interval spanned_system_rank_interval () const override;

  void set_bound (Direction d, Grob *);
  // accepts_as_bound_...() are used in the implementation of set_bound ().
  virtual bool accepts_as_bound_item (const Item *) const;
  virtual bool accepts_as_bound_paper_column (const Paper_column *) const;
  Item *get_bound (Direction d) const;
  Drul_array<Item *> get_bounds () const;

  Spanner (SCM);
  Spanner (Spanner const &);
  Spanner *original () const
  {
    // safe: if there is an original, it is because this was cloned from it
    return static_cast<Spanner *> (Grob::original ());
  }
  bool is_broken () const;
  void do_break ();
  Real spanner_length () const;

  static bool less (Spanner *const &, Spanner *const &);
  Spanner *find_broken_piece (System *) const override;

  Spanner *pure_find_visible_prebroken_piece (vsize, vsize) const final override
  {
    return const_cast<Spanner *> (this);
  }

  void derived_mark () const override;
  System *get_system () const override;

  SCM get_cached_pure_property (SCM sym, vsize start, vsize end);
  void cache_pure_property (SCM sym, vsize start, vsize end, SCM value);
  Spanner *make_sticky_same_type (Engraver *eng, SCM type, SCM cause,
                                  char const *file, int line,
                                  char const *fun) override;

protected:
  void set_my_columns ();
  Spanner *clone () const override { return new Spanner (*this); }
  void do_break_processing () override;
  bool fast_substitute_grob_array (SCM sym, Grob_array const *);
};

void add_bound_item (Spanner *, Grob *);

bool spanner_less (Spanner *s1, Spanner *s2);
#endif
