/*
  hairpin.hh -- declare Hairpin

  source file of the GNU LilyPond music typesetter

  (c) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef HAIRPIN_HH
#define HAIRPIN_HH

#include "grob-interface.hh"
#include "lily-proto.hh"

struct Hairpin
{
public:
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (height, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_height, (SCM, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  static void consider_suicide (Spanner*);
  DECLARE_GROB_INTERFACE();
};

#endif // HAIRPIN_HH
