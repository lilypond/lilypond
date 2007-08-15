/*
  clef.hh -- declare Clef

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CLEF_HH
#define CLEF_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

struct Clef
{
  DECLARE_SCHEME_CALLBACK (calc_glyph_name, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);
};

#endif /* CLEF_HH */

