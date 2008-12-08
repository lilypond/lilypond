/*
  spaceable-grob.hh -- declare Spaceable_grob

  source file of the GNU LilyPond music typesetter

  (c) 2000--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPACEABLE_GROB_HH
#define SPACEABLE_GROB_HH

#include "lily-proto.hh"
#include "grob-interface.hh"
#include "spring.hh"

struct Spaceable_grob
{
  /// set a minimum distance
  static void add_rod (Grob *me, Grob *to, Real distance);
  static void add_spring (Grob *me, Grob *to, Spring sp);
  static Spring get_spring (Grob *me, Grob *other);

  DECLARE_GROB_INTERFACE();
  static SCM get_minimum_distances (Grob *);
  static SCM get_ideal_distances (Grob *);
};

#endif /* SPACEABLE_GROB_HH */

