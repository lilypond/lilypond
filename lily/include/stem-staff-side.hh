/*   
  stem-staff-side.hh -- declare Stem_staff_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef Stem_STAFF_SIDE_HH
#define Stem_STAFF_SIDE_HH

#include "staff-side.hh"
/**
   Position self, analogous to Staff_side_item, but use Stem direction
   for determining direction.

   Properties:

   padding :: Real

   Padding in staffline leading
   
 */
class Stem_staff_side_item : public Staff_side_item
{
  Stem *stem_l_;
public:
  Direction relative_dir_;

  void set_stem (Stem*);
  Stem_staff_side_item ();
protected:
  virtual Direction get_default_direction ()const;
  virtual void do_pre_processing ();
  virtual void do_substitute_element_pointer (Score_element*o,Score_element*e);
};

#endif /* Stem_STAFF_SIDE_HH */

