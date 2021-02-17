/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2021 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#ifndef ITEM_HH
#define ITEM_HH

#include "grob.hh"

/**
   A horizontally fixed size element of the score.

   Item is the datastructure for printables whose width is known
   before the spacing is calculated
*/
class Item : public Grob
{
  Drul_array<Item *> broken_to_drul_;

  OVERRIDE_CLASS_NAME (Item);
public:
  Item (SCM);
  Item (Item const &);

  Item *clone () const override { return new Item (*this); }
  Item *original () const
  {
    // safe: if there is an original, it is because this was cloned from it
    return static_cast<Item *> (Grob::original ());
  }

  static bool is_non_musical (Grob *);
  static bool break_visible (Grob *);

  bool is_broken () const;

  Direction break_status_dir () const;

  Item *find_prebroken_piece (Direction d) const
  {
    return !d ? const_cast<Item *> (this) : broken_to_drul_[d];
  }

  Item *find_broken_piece (System *) const override;
  System *get_system () const override;
  virtual Paper_column *get_column () const;
  void handle_prebroken_dependencies () override;
  Interval_t<int> spanned_rank_interval () const override;
  Interval pure_y_extent (Grob *ref, vsize start, vsize end) override;
  virtual void cache_pure_height (Interval height);
  bool internal_set_as_bound_of_spanner (Spanner *, Direction) override;
protected:
  void break_breakable_item (System *) override;
  void copy_breakable_items ();
  void derived_mark () const override;

  bool cached_pure_height_valid_;
  Interval cached_pure_height_;
};

Interval_t<Moment> spanned_time_interval (Item *l, Item *r);

#endif
