/*
  spring.hh -- declare Spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "smobs.hh"

class Spring
{
  Real distance_;
  Real min_distance_;

  Real inverse_stretch_strength_;
  Real inverse_compress_strength_;

  Real blocking_force_;

  void update_blocking_force ();

  DECLARE_SIMPLE_SMOBS (Spring);
public:
  Spring ();
  Spring (Real distance, Real min_distance);

  Real distance () const {return distance_;}
  Real min_distance () const {return min_distance_;}
  Real inverse_stretch_strength () const {return inverse_stretch_strength_;}
  Real inverse_compress_strength () const {return inverse_compress_strength_;}
  Real blocking_force () const {return blocking_force_;}
  
  Real length (Real f) const;

  void set_distance (Real);
  void set_min_distance (Real);
  void set_inverse_stretch_strength (Real);
  void set_inverse_compress_strength (Real);
  void set_blocking_force (Real);
  void set_default_strength ();

  void operator*= (Real);
  bool operator> (Spring const&) const;
};
DECLARE_UNSMOB (Spring, spring);

Spring merge_springs (vector<Spring> const &springs);

#endif /* SPRING_HH */

