/*
  hyphen-spanner.hh -- part of GNU LilyPond

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_SPANNER_HH
#define HYPHEN_SPANNER_HH

#include "spanner.hh"

/** 
  centred hyphen 

  A centred hyphen is a simple line between lyrics used to
  divide syllables.

  The length of the hyphen line should stretch based on the
  size of the gap between syllables.

properties:

  thickness -- thickness of line (in stafflinethickness)

  height -- vertical offset  (in staffspace)

  minimum-length -- try to make the hyphens at least this long. Also works
    as a scaling parameter for the length

  word-space -- elongate left by this much (FIXME: cumbersome semantics)
  
  */
struct Hyphen_spanner 
{
public:
  Spanner* elt_l_;
  Hyphen_spanner  (Spanner*);
  void set_textitem (Direction, Score_element*);
  static SCM brew_molecule (SCM);
};

#endif // HYPHEN_SPANNER_HH

