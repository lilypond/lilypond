/*
  bar.hh -- part of GNU LilyPond

  (c) 1996--2007 Han-Wen Nienhuys
*/

#ifndef BAR_HH
#define BAR_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
   A vertical bar.
*/
class Bar_line
{
public:
  static bool has_interface (Grob *);

  static Stencil dashed_bar_line (Grob *me, Real h, Real thick);
  static Stencil compound_barline (Grob *, string, Real height, bool rounded);
  static Stencil simple_barline (Grob *, Real wid, Real height, bool rounded);
  DECLARE_SCHEME_CALLBACK (calc_bar_size, (SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
};
#endif // BAR_HH

