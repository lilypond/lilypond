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
  Drul_array<Item*> broken_to_drul_;


public:

  /// I am really to be broken?
  bool breakable_b () const;
  bool broken_b () const;
  bool broken_original_b () const;
  
  Direction break_status_dir () const;
  
  Item * find_broken_piece (Direction) const;
  Score_element * find_broken_piece (Line_of_score*) const;    

  Item();
  Real hpos_f() const;
  
  virtual Line_of_score * line_l() const;
  virtual Paper_column * column_l () const;
    
  static int left_right_compare (Item const *, Item const*);
  
  Item (Item const &);
protected:
  virtual void do_breakable_col_processing();
  virtual void handle_prebroken_dependencies();
  //virtual void handle_prebroken_dependents ();

  void copy_breakable_items();
};



#endif
