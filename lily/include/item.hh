/*
  item.hh -- declare Item

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/
#ifndef ITEM_HH
#define ITEM_HH

#include "box.hh"
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
  Item (SCM, Object_key const *);
  Item (Item const &, int count);

  virtual Grob *clone (int count) const;

  static bool is_non_musical (Grob *);
  bool is_broken () const;
  bool pure_is_visible (int start, int end) const;

  Direction break_status_dir () const;

  Item *find_prebroken_piece (Direction) const;
  Grob *find_broken_piece (System *) const;
  virtual System *get_system () const;
  virtual Paper_column *get_column () const;
  virtual void handle_prebroken_dependencies ();
  virtual Interval_t<int> spanned_rank_iv ();
  static bool has_interface (Grob *);
protected:
  virtual void discretionary_processing ();
  void copy_breakable_items ();
  virtual void derived_mark () const;
};

#endif
