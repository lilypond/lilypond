/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
   This is a barline that is spanned across other bar lines.  This is
   the implementation of the long barlines that occur in orchestral
   score and other multi-staff music.
*/
class Span_bar
{
public:
  static void set_interface (Grob*);
  static bool has_interface (Grob*);  
  static Interval get_spanned_interval (Grob*);
  static void add_bar (Grob*,Grob*);
  static void evaluate_glyph (Grob*);
  static void evaluate_empty (Grob*);
  DECLARE_SCHEME_CALLBACK (width_callback, (SCM smob, SCM axis));
  DECLARE_SCHEME_CALLBACK (get_bar_size, (SCM ));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK (center_on_spanned_callback, (SCM element, SCM axis));
};

#endif // SPAN_BAR_HH
