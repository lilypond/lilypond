/*   
  text-item.hh -- declare markup functions

  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
 */

#ifndef TEXT_ITEM
#define TEXT_ITEM


#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stencil.hh"

class Text_interface
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (interpret_markup, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (interpret_string, (SCM, SCM, SCM, SCM));
  static bool has_interface (Grob*);
  static bool markup_p (SCM) ;
};

#endif /* TEXT_ITEM */
