/*
  volta-spanner.hh -- part of GNU LilyPond

  (c) 1997--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef VOLTA_SPANNER_HH
#define VOLTA_SPANNER_HH


#include "spanner.hh"

/** 
*/

class Volta_spanner
{
public:
  static void set_interface (Grob*);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  static void add_column (Grob*, Grob*col);
  static void add_bar (Grob*me, Item*bar);
};

#endif // VOLTA_SPANNER_HH

