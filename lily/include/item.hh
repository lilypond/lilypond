/*
  item.hh -- declare Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef ITEM_HH
#define ITEM_HH


#include "box.hh"
#include "grob.hh"
#include "drul-array.hh"


/**
  A horizontally fixed size element of the score.

  Item is the datastructure for printables whose width is known
  before the spacing is calculated

*/
class Item : public Grob
{
  Drul_array<Item*> broken_to_drul_;

public:
  VIRTUAL_COPY_CONS (Grob);
  Item (SCM);
  Item (Item const &);

  static bool breakable_b (Grob *);
  bool broken_b () const;
  
  Direction break_status_dir () const;
  
  Item *find_prebroken_piece (Direction) const;
  Grob *find_broken_piece (System *) const;    

  virtual System *get_system () const;
  virtual Paper_column *get_column () const;
  virtual void handle_prebroken_dependencies ();
  static  bool has_interface (Grob *);
protected:
  virtual void discretionary_processing ();
  void copy_breakable_items ();
  virtual SCM do_derived_mark ();
};



#endif
