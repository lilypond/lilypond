/*
  spring.hh -- declare Spring, Column_spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "smobs.hh"

struct Spring
{
  Grob *other_;
  Real distance_;
  Real min_distance_;

  Real inverse_stretch_strength_;
  Real inverse_compress_strength_;

  DECLARE_SIMPLE_SMOBS (Spring);
public:
  Spring ();
};
DECLARE_UNSMOB (Spring, spring);

#endif /* SPRING_HH */

