/*   
  script.hh -- declare Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

/**
   Articulation marks (and the like) that are attached to notes/stems.
   Needs support from Staff_side for proper operation.  Staff_side
   handles the positioning.
*/
class Script
{
public:
  static Molecule get_molecule (Score_element*,Direction d);
  static void set_interface (Score_element*);
  static bool  has_interface (Score_element*);
  DECLARE_SCHEME_CALLBACK(brew_molecule, (SCM ));
  DECLARE_SCHEME_CALLBACK(after_line_breaking, (SCM ));
};

#endif /* Stem_SCRIPT_HH */

