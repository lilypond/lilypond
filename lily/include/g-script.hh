/*   
  g-script.hh -- declare G_script
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_STEM_SCRIPT_HH
#define G_STEM_SCRIPT_HH

#include "item.hh"
#include "drul-array.hh"

/**
   articulation marks (and the like) that are attached to notes/stems.
   Needs support from G_staff_side for proper operation.
 */
class G_script : public Item
{
  G_staff_side_item * staff_side_l_;

  Molecule get_molecule (Direction d) const;
public:
  Drul_array<String> indices_drul_;
  G_script ();
  void set_staff_side (G_staff_side_item*);

protected:
virtual void do_print () const;
  virtual void do_substitute_element_pointer (Score_element*o,
					      Score_element*n);
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  Molecule* do_brew_molecule_p () const;
};

#endif /* G_STEM_SCRIPT_HH */

