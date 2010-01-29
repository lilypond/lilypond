/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

  DECLARE_CLASSNAME(Item);
public:
  Item (SCM);
  Item (Item const &);

  virtual Grob *clone () const;

  static bool is_non_musical (Grob *);
  static bool break_visible(Grob *);
  static bool less (Grob * const&, Grob * const&);
  bool is_broken () const;
  bool pure_is_visible (int start, int end) const;

  Direction break_status_dir () const;

  Item *find_prebroken_piece (Direction) const;
  Grob *find_broken_piece (System *) const;
  virtual System *get_system () const;
  virtual Paper_column *get_column () const;
  virtual void handle_prebroken_dependencies ();
  virtual Interval_t<int> spanned_rank_interval () const;
  virtual Interval pure_height (Grob *ref, int start, int end);
  DECLARE_GROB_INTERFACE();
protected:
  virtual void discretionary_processing ();
  void copy_breakable_items ();
  virtual void derived_mark () const;

  bool cached_pure_height_valid_;
  Interval cached_pure_height_;
};

Interval_t<Moment> spanned_time_interval (Item *l, Item *r);

#endif
