/*
  chord-name.hh -- declare Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_HH
#define CHORD_NAME_HH

#include "stencil.hh"

class Chord_name
{
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  static bool has_interface (Grob *);
};

#endif // CHORD_NAME_HH
