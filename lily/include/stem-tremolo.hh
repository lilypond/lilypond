/*   
  stem-tremolo.hh -- declare Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ABBREV_HH
#define ABBREV_HH

#include "item.hh"


class Stem_tremolo : public Item {
protected:
  Stem * stem_l () const;
  virtual void do_print () const;
  virtual Molecule *do_brew_molecule_p () const;

  static Interval dim_callback (Dimension_cache const*);
public:
  int abbrev_flags_i_;
  Stem_tremolo ();
  void set_stem (Stem *);
};

#endif /* ABBREV_HH */

