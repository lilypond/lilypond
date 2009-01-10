/*
  clef.hh -- declare Clef

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef CLEF_HH
#define CLEF_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Clef
{
  DECLARE_SCHEME_CALLBACK (calc_glyph_name, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif /* CLEF_HH */

