/*
  hairpin.hh -- declare Hairpin

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef HAIRPIN_HH
#define HAIRPIN_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
/**
   The hairpin symbol.
*/
struct Hairpin
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  static void consider_suicide (Spanner*);
  static bool has_interface (Grob *);
};

#endif // HAIRPIN_HH
