/*
  hyphen-spanner.hh -- part of GNU LilyPond

  (c) 1999--2006 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_SPANNER_HH
#define HYPHEN_SPANNER_HH

#include "spanner.hh"

struct Lyric_hyphen
{
public:
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  bool has_interface (Grob *);
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif // HYPHEN_SPANNER_HH

