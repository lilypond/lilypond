/*   
  text-item.hh -- declare Text_item

  source file of the GNU LilyPond music typesetter
  
  (c)  1998--2003 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef TEXT_ITEM
#define TEXT_ITEM


#include "lily-proto.hh"
#include "lily-guile.hh"
#include "molecule.hh"

class Text_item
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  DECLARE_SCHEME_CALLBACK (interpret_markup, (SCM,SCM, SCM));
  static bool has_interface (Grob*);
  static bool markup_p (SCM) ;

};


  


#endif /* TEXT_ITEM */
