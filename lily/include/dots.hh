/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DOTS_HH
#define DOTS_HH

#include "lily-proto.hh"
#include "lily-guile.hh"

class Dots
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  static bool has_interface (Grob *);
};

#endif // DOTS_HH
