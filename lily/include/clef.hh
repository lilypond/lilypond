/*   
  clef.hh -- declare Clef
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CLEF_HH
#define CLEF_HH
#include "lily-guile.hh"
#include "lily-proto.hh"

struct Clef 
{
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  static bool has_interface (Grob*);
  
};


#endif /* CLEF_HH */

