/*
  System-start-delimiter.hh -- declare System_start_delimiter

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SYSTEM_START_DELIMITER_HH
#define SYSTEM_START_DELIMITER_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/*
  Braces/brackets across staves.
*/
class System_start_delimiter
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));

  DECLARE_GROB_INTERFACE();
  static void try_collapse (Grob *);
  static Stencil staff_bracket (Grob *, Real);
  static Stencil old_staff_bracket (Grob *, Real);
  static Stencil staff_brace (Grob *, Real);
  static Stencil simple_bar (Grob *, Real);
  static Stencil line_bracket (Grob *, Real);
  
};

#endif /* SYSTEM_START_DELIMITER_HH */

