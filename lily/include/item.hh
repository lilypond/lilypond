/*
  item.hh -- declare Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#ifndef ITEM_HH
#define ITEM_HH


#include "box.hh"
#include "string.hh"
#include "score-element.hh"
#include "drul-array.hh"
#include "protected-scm.hh"

/**
  A horizontally fixed size element of the score.

  Item is the datastructure for printables whose width is known
  before the spacing is calculated

  NB. This doesn't mean an Item has to initialize the output field before
  spacing calculation. 
  

  
  @signature
  visibility_lambda :: int -> (bool . bool)
     
  @in
  break direction
     
  @out
  (transparent, empty) cons
     
   */
class Item : public virtual Score_element {
  void do_break ();
  void try_visibility_lambda ();

public:
  Link_array<Spanner> attached_span_l_arr_;
  Drul_array<Item*> broken_to_drul_;
  Item *unbroken_original_l_;

  /// should be put in a breakable col.
  bool breakable_b_;

  /// I am really to be broken?
  virtual bool breakable_b () const;
  
  Direction break_status_dir_;
  int break_priority_i_;
  
  /// nobreak = 0, pre = -1, post = 1
  Direction break_status_dir() const;
  Item * find_prebroken_piece (Direction) const;
  Item * find_prebroken_piece (Line_of_score*) const;    

  Item();
  Real hpos_f() const;
  
  virtual Line_of_score * line_l() const;
  virtual Paper_column * column_l () const;
    
  static int left_right_compare (Item const *, Item const*);
  
  Item (Item const &);
protected:
  virtual void do_breakable_col_processing();
  virtual void handle_prebroken_dependencies();
  virtual void do_print() const;
  virtual bool linked_b() const;

  virtual void handle_prebroken_dependents ();

  void copy_breakable_items();
};



#endif
