/*
  item.hh -- declare Item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  
  
  Element properties
  
  visibility-lambda -- a function that takes the break
  direction and returns a (transparent, empty) cons

  breakable -- boolean indicating if this is a breakable item (clef,
  barline, key sig, etc.)

   */
class Item : public Score_element
{
  Drul_array<Item*> broken_to_drul_;

public:
  VIRTUAL_COPY_CONS(Score_element);
  Item (SCM);
  Item (Item const &);

  bool breakable_b () const;
  bool broken_b () const;
  
  Direction break_status_dir () const;
  
  Item * find_prebroken_piece (Direction) const;
  Score_element * find_broken_piece (Line_of_score*) const;    

  virtual Line_of_score * line_l() const;
  virtual Paper_column * column_l () const;
  virtual void handle_prebroken_dependencies ();
protected:
  virtual void discretionary_processing ();
  void copy_breakable_items();
};



#endif
