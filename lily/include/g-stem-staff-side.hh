/*   
  g-stem-staff-side.hh -- declare G_stem_staff_side
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef G_STEM_STAFF_SIDE_HH
#define G_STEM_STAFF_SIDE_HH

#include "g-staff-side.hh"
/**
   Position self, analogous to G_staff_side_item, but use Stem direction
   for determining direction.

   Properties:

   padding :: Real

   Padding in staffline leading
   
 */
class G_stem_staff_side_item : public G_staff_side_item
{
  Stem *stem_l_;
public:
  Direction relative_dir_;

  void set_stem (Stem*);
  G_stem_staff_side_item ();
protected:
  virtual Direction get_default_direction ()const;
  virtual void do_pre_processing ();
  virtual void do_substitute_element_pointer (Score_element*o,Score_element*e);
};

#endif /* G_STEM_STAFF_SIDE_HH */

