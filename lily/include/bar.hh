/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
  A vertical bar.
 */
class Bar
{
public:
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);
  static Molecule compound_barline (Score_element*, String, Real height) ;
  static Molecule simple_barline (Score_element*, Real wid, Real height) ;      
  DECLARE_SCHEME_CALLBACK(get_staff_bar_size, (SCM ));
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  DECLARE_SCHEME_CALLBACK(before_line_breaking, (SCM ));
};
#endif // BAR_HH

