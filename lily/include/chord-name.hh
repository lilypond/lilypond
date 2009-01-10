/*
  chord-name.hh -- declare Chord_name

  source file of the GNU LilyPond music typesetter

  (c) 1999--2009 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef CHORD_NAME_HH
#define CHORD_NAME_HH

#include "stencil.hh"
#include "grob-interface.hh"

class Chord_name
{
public:
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  DECLARE_GROB_INTERFACE();
};

#endif // CHORD_NAME_HH
