/*   
  System-start-delimiter.hh -- declare System_start_delimiter
     
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  DECLARE_SCHEME_CALLBACK (print, (SCM ));
  
  
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));
  static void try_collapse (Grob*);
  static Stencil staff_bracket (Grob*, Real) ;
  static Stencil staff_brace (Grob*, Real) ;
  static Stencil simple_bar (Grob*, Real) ;
};

#endif /* SYSTEM_START_DELIMITER_HH */

