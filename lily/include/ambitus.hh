/*
  ambitus.hh

  source file of the GNU LilyPond music typesetter

  (c) 2000--2009 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef AMBITUS_HH
#define AMBITUS_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

struct Ambitus
{
  DECLARE_SCHEME_CALLBACK (print, (SCM smob));
  DECLARE_GROB_INTERFACE();
  static Slice get_positions (Grob *);
  static Interval head_width (Grob *me, Grob *common);
};

#endif // AMBITUS_HH

