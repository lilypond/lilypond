/*
  arpegio.hh -- declare Arpeggio
  
  source file of the GNU LilyPond music typesetter

  (c) 2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef ARPEGGIO_HH
#define ARPEGGIO_HH

#include "lily-guile.hh"
#include "lily-proto.hh"
/*
  properties:

  stems -- list of stem objects, corresponding to the notes that the
  arp has to be before.  */
class Arpeggio
{
public:
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  DECLARE_SCHEME_CALLBACK(width_callback, (SCM,SCM));
  static bool has_interface (Score_element*);
};

#endif /* ARPEGGIO_HH */

