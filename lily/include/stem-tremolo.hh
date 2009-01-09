/*
  stem-tremolo.hh -- declare Abbreviation

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef ABBREV_HH
#define ABBREV_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Stem_tremolo
{
public:

  DECLARE_GROB_INTERFACE();
  DECLARE_SCHEME_CALLBACK (calc_slope, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_width, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_style, (SCM));
  static Stencil raw_stencil (Grob *, Real slope, Direction stemdir);
  static Stencil translated_stencil (Grob*, Real slope);
  static Stencil untranslated_stencil (Grob*, Real slope);
  static Real get_beam_translation (Grob *me);
  static Real vertical_length (Grob *me);
};

#endif /* ABBREV_HH */

