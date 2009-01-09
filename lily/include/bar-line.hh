/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--2009 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Bar_line
{
public:
  DECLARE_GROB_INTERFACE();

  static Stencil dashed_bar_line (Grob *me, Real h, Real thick);
  static Stencil tick_bar_line (Grob *me, Real h, bool rounded);
  static Stencil compound_barline (Grob *, string, Real height, bool rounded);
  static Stencil simple_barline (Grob *, Real wid, Real height, bool rounded);
  static Interval bar_y_extent (Grob *, Grob *);
  DECLARE_SCHEME_CALLBACK (calc_bar_extent, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_bar_size, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_anchor, (SCM));
};
#endif // BAR_HH

