/*   
  script.hh -- declare Script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
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
  
protected:
  virtual void before_line_breaking ();
  virtual void after_line_breaking ();
  Molecule do_brew_molecule () const;
};

#endif /* Stem_SCRIPT_HH */

