/*
  span-bar.hh -- declare Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef SPAN_BAR_HH
#define SPAN_BAR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
   This is a barline that is spanned across other bar lines.  This is
   the implementation of the long barlines that occur in orchestral
   score and other multi-staff music.

   TODO: Is this usable for other stuff besides barlines? We only have
   to span a Score_element.  Perhaps this can be used for large time
   sigs?
*/
class Span_bar
{
public:
  static void set_interface (Score_element*);
  static bool has_interface (Score_element*);  
  static Interval get_spanned_interval (Score_element*);
  static void add_bar (Score_element*,Score_element*);
  static void evaluate_glyph (Score_element*);
  static void evaluate_empty (Score_element*);
  DECLARE_SCHEME_CALLBACK(width_callback, (SCM smob, SCM axis));
  DECLARE_SCHEME_CALLBACK(get_bar_size, (SCM ));
  DECLARE_SCHEME_CALLBACK(before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK(center_on_spanned_callback, (SCM element, SCM axis));
};

#endif // SPAN_BAR_HH
