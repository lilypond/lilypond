/*   
  g-text-item.hh -- declare G_text_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_TEXT_ITEM_HH
#define G_TEXT_ITEM_HH

#include "item.hh"

/**
   Print a text in specified style.
 */
class G_text_item : public Item
{
public:
  String text_str_;
  String style_str_;

  G_text_item ();
  VIRTUAL_COPY_CONS (Score_element);
protected:
  virtual void do_print () const;
  virtual Molecule *do_brew_molecule_p () const;
};

#endif /* G_TEXT_ITEM_HH */

