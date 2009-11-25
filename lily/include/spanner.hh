/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2009 Han-Wen Nienhuys

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
class Spanner : public Grob
{
  Drul_array<Item *> spanned_drul_;
  vsize break_index_;

  DECLARE_CLASSNAME(Spanner);

public:
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_SCHEME_CALLBACK (bounds_width, (SCM));
  DECLARE_SCHEME_CALLBACK (kill_zero_spanned_time, (SCM));

  vector<Spanner*> broken_intos_;

  vsize get_break_index () const;
  Spanner *broken_neighbor (Direction d) const;
  
  // todo: move to somewhere else.
  Real get_broken_left_end_align () const;
  void substitute_one_mutable_property (SCM sym, SCM val);

  Interval_t<Moment> spanned_time () const;
  virtual Interval_t<int> spanned_rank_interval () const;
  void set_bound (Direction d, Grob *);
  Item *get_bound (Direction d) const;

  Spanner (SCM);
  Spanner (Spanner const &);
  bool is_broken () const;
  void do_break ();
  Real spanner_length () const;

  static int compare (Spanner *const &, Spanner *const &);
  static bool less (Spanner *const &, Spanner *const &);
  virtual Grob *find_broken_piece (System *) const;
  virtual void derived_mark () const;
  DECLARE_GROB_INTERFACE();
  virtual System *get_system () const;

protected:
  void set_my_columns ();
  virtual Grob *clone () const;
  virtual void do_break_processing ();
  bool fast_substitute_grob_array (SCM sym, Grob_array *);
};

void add_bound_item (Spanner *, Grob *);

bool spanner_less (Spanner *s1, Spanner *s2);
int broken_spanner_index (Spanner const *sp);
#endif
