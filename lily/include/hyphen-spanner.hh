/*
  hyphen-spanner.hh -- part of GNU LilyPond

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_SPANNER_HH
#define HYPHEN_SPANNER_HH

#include "spanner.hh"

struct Hyphen_spanner 
{
public:
  Spanner* elt_l_;
  Hyphen_spanner (Spanner*);
  void set_textitem (Direction, Grob*);
  bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
};

#endif // HYPHEN_SPANNER_HH

