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
  DECLARE_SCHEME_CALLBACK (calc_slope, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_width, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_style, (SCM));
  static Stencil rotated_box (Real slope, Real width, Real thick, Real blot);
  static Stencil raw_stencil (Grob *, Real, Direction stemdir);
  static Real get_beam_translation (Grob *me);
};

#endif /* ABBREV_HH */

