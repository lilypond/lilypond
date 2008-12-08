/*
  item.hh -- declare Item

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
