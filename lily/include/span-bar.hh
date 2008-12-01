/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

/**
   This is a barline that is spanned across other bar lines.  This is
   the implementation of the long barlines that occur in orchestral
   score and other multi-staff music.
*/
class Span_bar
{
public:

  DECLARE_GROB_INTERFACE();
  static Interval get_spanned_interval (Grob *);
  static void add_bar (Grob *, Grob *);
  static void evaluate_glyph (Grob *);
  DECLARE_SCHEME_CALLBACK (width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (calc_bar_size, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_glyph_name, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (center_on_spanned_callback, (SCM element));
};

#endif // SPAN_BAR_HH
