/*   
accidental-placement.hh -- declare  Accidental_placement

source file of the GNU LilyPond music typesetter

(c) 2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>

 */

#ifndef ACCIDENTAL_PLACEMENT_HH
#define ACCIDENTAL_PLACEMENT_HH

#include "grob.hh"

class Accidental_placement
{
public:
  DECLARE_SCHEME_CALLBACK (alignment_callback, (SCM element, SCM axis));
  DECLARE_SCHEME_CALLBACK (extent_callback, (SCM element, SCM axis));  
  static void add_accidental (Grob *,Grob* );

  
  static SCM position_accidentals (Grob* );
  static bool has_interface (Grob*);
};
#endif /* ACCIDENTAL_PLACEMENT_HH */

