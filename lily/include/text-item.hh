/*   
  text-item.hh -- declare Text_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Text_ITEM_HH
#define Text_ITEM_HH

#include "item.hh"

/**
   Print a text in specified style.
 */
class Text_item : public Item
{
public:
  String text_str_;
  String style_str_;

  Text_item ();
  VIRTUAL_COPY_CONS (Score_element);
protected:
  virtual void do_print () const;
  virtual Molecule *do_brew_molecule_p () const;
};

#endif /* Text_ITEM_HH */

