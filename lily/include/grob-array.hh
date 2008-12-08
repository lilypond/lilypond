/*
  grob-array.hh -- declare Grob_array

  source file of the GNU LilyPond music typesetter

  (c) 2005--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef GROB_ARRAY_HH
#define GROB_ARRAY_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "std-vector.hh"

class Grob_array
{
  vector<Grob*> grobs_;
  bool ordered_;

  DECLARE_SIMPLE_SMOBS (Grob_array);

  Grob_array ();
public:
  bool ordered () const { return ordered_; }
  void set_ordered (bool b) { ordered_ = b; }
  Item *item (vsize i);
  Spanner *spanner (vsize i);
  Grob *grob (vsize i) { return grobs_.at (i); }
  vsize size () const { return grobs_.size (); }
  bool empty () const;
  void remove_duplicates ();
  void clear ();
  void add (Grob *x) { grobs_.push_back (x); }
  void set_array (vector<Grob*> const &src);
  vector<Grob*> &array_reference ();
  vector<Grob*> const &array () const;
  static SCM make_array ();
};

DECLARE_UNSMOB (Grob_array, grob_array);

vector<Grob*> const &ly_scm2link_array (SCM x);
SCM grob_list_to_grob_array (SCM lst);

#endif /* GROB_ARRAY_HH */

