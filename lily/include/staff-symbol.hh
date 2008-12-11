/*
  staff-symbol.hh -- declare Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef STAFF_SYMBOL_HH
#define STAFF_SYMBOL_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   TODO: add linethickness as parameter.
*/
class Staff_symbol
{
public:
  static Real staff_space (Grob *);
  static Real get_line_thickness (Grob *);
  static Real get_ledger_line_thickness (Grob *);
  
  static int get_steps (Grob *);
  static int line_count (Grob *);
  static bool on_line (Grob *me, int pos);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));  
  DECLARE_GROB_INTERFACE();
};
#endif // STAFF_SYMBOL_HH
