/*
  hyphen-spanner.hh -- part of GNU LilyPond

  (c) 1999 Glen Prideaux <glenprideaux@iname.com>
*/

#ifndef HYPHEN_SPANNER_HH
#define HYPHEN_SPANNER_HH

#include "spanner.hh"

/** 
  centred hyphen 

  A centred hyphen is a simple line between lyrics used to
  divide syllables.

  The length of the hyphen line should stretch based on the
  size of the gap between syllables.
  */
class Hyphen_spanner : public Spanner
{
public:
  Hyphen_spanner (SCM);
  void set_textitem (Direction, Item*);
 static SCM scheme_molecule (SCM);
  

protected:
  virtual Molecule do_brew_molecule () const;
  Interval do_height () const;

  void after_line_breaking ();
 
  VIRTUAL_COPY_CONS (Score_element);

  Drul_array<Real> dx_f_drul_;
};

#endif // HYPHEN_SPANNER_HH

