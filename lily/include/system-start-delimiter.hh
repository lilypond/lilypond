/*   
  System-start-delimiter.hh -- declare System_start_delimiter
     
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SYSTEM_START_DELIMITER_HH
#define SYSTEM_START_DELIMITER_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/*
  Braces/brackets across staves.
 */
class System_start_delimiter
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  
  static void set_interface (Grob*me);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));
  static void try_collapse (Grob*);
  static Molecule staff_bracket (Grob*,Real) ;
  static Molecule staff_brace (Grob*,Real) ;
  static Molecule simple_bar (Grob*,Real) ;
};

#endif /* SYSTEM_START_DELIMITER_HH */

