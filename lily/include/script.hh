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
  Staff_side_item * staff_side_l_;

  Molecule get_molecule (Direction d) const;
public:
  Script ();
  void set_staff_side (Staff_side_item*);

protected:
  virtual void do_print () const;
  virtual void do_substitute_element_pointer (Score_element*o,
					      Score_element*n);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  Molecule* do_brew_molecule_p () const;
};

#endif /* Stem_SCRIPT_HH */

