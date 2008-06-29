/*
  rest.hh -- declare Rest

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef REST_HH
#define REST_HH

#include "grob-interface.hh"

class Grob;

class Rest
{
public:
  DECLARE_SCHEME_CALLBACK (y_offset_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_cross_staff, (SCM));
  DECLARE_GROB_INTERFACE();
  static string glyph_name (Grob *, int, string, bool);
  static SCM brew_internal_stencil (Grob*, bool);
  static SCM generic_extent_callback (Grob*, Axis);
  DECLARE_SCHEME_CALLBACK (polyphonic_offset_callback, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (width, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
};
#endif // REST_HH
