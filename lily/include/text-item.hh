/*   
  text-item.hh -- declare Text_item

  source file of the GNU LilyPond music typesetter
  
 (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef TEXT_ITEM
#define TEXT_ITEM

#include "lily-guile.hh"
#include "molecule.hh"

class Text_item
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static Molecule text2molecule (Score_element *me, SCM text, SCM properties);
  static Molecule string2molecule (Score_element *me, SCM text, SCM properties);
  static Molecule markup_sentence2molecule (Score_element *me, SCM markup_sentence, SCM properties);
  static Molecule lookup_character (Score_element *me, SCM font_name, SCM text);
  static Molecule lookup_text (Score_element *me, SCM font_name, SCM char_name);
};

#endif /* TEXT_ITEM */
