/*
  stem-tremolo.hh -- declare Abbreviation

  source file of the GNU LilyPond music typesetter

  (c) 1997--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ABBREV_HH
#define ABBREV_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Stem_tremolo
{
public:

  static bool has_interface (Grob *);
  DECLARE_SCHEME_CALLBACK (dim_callback, (SCM smob));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  static Stencil raw_stencil (Grob *);
};

#endif /* ABBREV_HH */

