/*
  span-arpegio.hh -- declare Span_arpeggio
  
  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef SPAN_ARPEGGIO_HH
#define SPAN_ARPEGGIO_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
#include "interval.hh"

class Span_arpeggio
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  static bool has_interface (Score_element*);
};

#endif /* SPAN_ARPEGGIO_HH */

