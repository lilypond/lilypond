/*   
  clef.hh -- declare 
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef CLEF_HH
#define CLEF_HH
#include "lily-guile.hh"
#include "lily-proto.hh"

/**
  Set a clef in a staff.

  properties:

  non-default -- not set because of existence of a bar?

  change -- is this a change clef (smaller size)?

  glyph -- a string determining what glyph is typeset
  
 */
struct Clef 
{
  static SCM before_line_breaking (SCM);
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);
};


#endif /* CLEF_HH */

