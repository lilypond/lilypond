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
  DECLARE_SCHEME_CALLBACK (text_to_molecule, (SCM,SCM, SCM));
  static Molecule text2molecule (Grob *me, SCM text, SCM properties);
  static Molecule string2molecule (Grob *me, SCM text, SCM properties);
  static Molecule markup_text2molecule (Grob *me, SCM markup_text, SCM properties);
  static bool has_interface (Grob*);
  
private:
  static Molecule lookup_character (Grob *me, Font_metric*, SCM char_name);
  static Molecule lookup_text (Grob *me, Font_metric*, SCM text);
};

bool new_markup_p (SCM) ;
SCM new_markup_brewer ();


#endif /* TEXT_ITEM */
