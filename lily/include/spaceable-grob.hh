/*   
  spaceable-grob.hh -- declare Spaceable_grob
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef SPACEABLE_GROB_HH
#define SPACEABLE_GROB_HH

#include "lily-guile.hh"
#include "lily-proto.hh"

struct Spaceable_grob
{
  /// set a minimum distance
  static void add_rod (Grob*me, Grob * to, Real distance);
  static void add_spring (Grob*me,Grob * to, Real dist, Real strength, bool);

  static void remove_interface (Grob*);
  static SCM get_minimum_distances (Grob*);
  static SCM get_ideal_distances (Grob*);
};

#endif /* SPACEABLE_GROB_HH */

