/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_HH
#define REST_HH

#include "lily-guile.hh"

class  Rest
{
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));
  static bool has_interface (Grob*);
  static String glyph_name (Grob*, int, String); 
  static SCM brew_internal_molecule (SCM);
  DECLARE_SCHEME_CALLBACK (extent_callback, (SCM,SCM));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
};
#endif // REST_HH
