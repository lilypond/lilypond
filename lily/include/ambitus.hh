/*
  ambitus.hh

  source file of the GNU LilyPond music typesetter

  (c) 2000--2007 Juergen Reuter <reuter@ipd.uka.de>
*/

#ifndef AMBITUS_HH
#define AMBITUS_HH

#include "lily-guile.hh"

class Grob;

struct Ambitus
{
  DECLARE_SCHEME_CALLBACK (print, (SCM smob));
  static bool has_interface (Grob *);
  static Slice get_positions (Grob *);
  static Interval head_width (Grob *me, Grob *common);
};

#endif // AMBITUS_HH

