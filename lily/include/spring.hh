/*   
  spring.hh -- declare Spring, Column_spring
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "drul-array.hh"
#include "smobs.hh"

struct Spring_smob
{
  Grob *other_;
  Real distance_f_;
  bool expand_only_b_;
  Real strength_f_;
  
  DECLARE_SIMPLE_SMOBS(Spring_smob,dummy);
public:
  SCM smobbed_copy () const;
  Spring_smob();
};
DECLARE_UNSMOB(Spring_smob, spring);

struct Spring{
  Drul_array<Item*> item_l_drul_;
  Real distance_f_;
  bool expand_only_b_;

  /*
    TODO: make 2 strengths: one for stretching, and one for shrinking.
  */
  Real strength_f_;
  void add_to_cols ();
  void set_to_cols ();
  Spring ();
};


#endif /* SPRING_HH */

