/*   
  text-item.hh -- declare Text_item
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  VIRTUAL_COPY_CONS (Score_element);
   static SCM scheme_molecule (SCM);
  
Text_item (SCM s);
protected:
  virtual Molecule do_brew_molecule () const;
};

#endif /* Text_ITEM_HH */

