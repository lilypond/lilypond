/*
  dots.hh -- declare Dots

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef DOTS_HH
#define DOTS_HH

#include "lily-proto.hh"
#include "grob-interface.hh"

class Dots
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif // DOTS_HH
