/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_HH
#define REST_HH

#include "lily-guile.hh"

class Grob;

class  Rest
{
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));
  static bool has_interface (Grob*);
  static String glyph_name (Grob*, int, String, bool); 
  static SCM brew_internal_stencil (SCM, bool);
  DECLARE_SCHEME_CALLBACK (extent_callback, (SCM,SCM));
  DECLARE_SCHEME_CALLBACK (polyphonic_offset_callback, (SCM,SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM ));
};
#endif // REST_HH
