/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef REST_HH
#define REST_HH

#include "lily-guile.hh"

/**
   A pause.
   
   Properties

   style -- string specifying glyph style
 */
class  Rest
{
public:
  static SCM after_line_breaking (SCM);
  static SCM brew_molecule (SCM);
};
#endif // REST_HH
