/*
  hyphen-spanner.hh -- part of GNU LilyPond

  (c) 1999--2009 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_SPANNER_HH
#define HYPHEN_SPANNER_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Lyric_hyphen
{
public:
  DECLARE_SCHEME_CALLBACK (set_spacing_rods, (SCM));
  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};

#endif // HYPHEN_SPANNER_HH

