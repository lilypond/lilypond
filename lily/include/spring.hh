/*
  spring.hh -- declare Spring, Column_spring

  source file of the GNU LilyPond music typesetter

  (c) 1999--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPRING_HH
#define SPRING_HH

#include "lily-proto.hh"
#include "smobs.hh"

struct Spring_smob
{
  Grob *other_;
  Real distance_;
  bool expand_only_b_;
  Real inverse_strength_;

  DECLARE_SIMPLE_SMOBS (Spring_smob);
public:
  Spring_smob ();
};
DECLARE_UNSMOB (Spring_smob, spring);

struct Spring
{
  Drul_array<Item *> item_drul_;
  Real distance_;
  bool expand_only_b_;

  /*
    TODO: make 2 strengths: one for stretching, and one for shrinking.
  */
  Real inverse_strength_;
  void add_to_cols ();
  void set_to_cols ();
  Spring ();
};

#endif /* SPRING_HH */

