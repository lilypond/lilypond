/*   
  System-start-delimiter.hh -- declare System_start_delimiter
     
  source file of the GNU LilyPond music typesetter
  
  (c) 2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SYSTEM_START_DELIMITER_HH
#define SYSTEM_START_DELIMITER_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/*
  Braces/brackets across staffs.
 */
class System_start_delimiter
{
public:
  static SCM brew_molecule (SCM);
  
  static void set_interface (Score_element*me);
  static bool has_interface (Score_element*);
  static SCM after_line_breaking (SCM);
  static void try_collapse (Score_element*);
  static Molecule staff_bracket (Score_element*,Real) ;
  static Molecule staff_brace (Score_element*,Real) ;
  static Molecule simple_bar (Score_element*,Real) ;
};

#endif /* SYSTEM_START_DELIMITER_HH */

