/*   
  script.hh -- declare Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SCRIPT_HH
#define SCRIPT_HH

#include "item.hh"
#include "drul-array.hh"

/**
   Articulation marks (and the like) that are attached to notes/stems.
   Needs support from Staff_side for proper operation.  Staff_side
   handles the positioning.
*/
class Script : public Item
{
  Molecule get_molecule (Direction d) const;
public:
  Script (SCM);
   static SCM brew_molecule (SCM);
  


  SCM member_before_line_breaking ();
  static SCM before_line_breaking (SCM);
  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);
  SCM member_brew_molecule () const;
};

#endif /* Stem_SCRIPT_HH */

