/*   
  abbrev.hh -- declare Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ABBREV_HH
#define ABBREV_HH

#include "item.hh"
class Abbreviation : public Item {
  Stem * stem_l_;
protected:
  virtual void do_print () const;
  virtual Molecule *do_brew_molecule_p () const;
  virtual void do_substitute_dependent (Score_element*, Score_element*);
public:
  int abbrev_flags_i_;
  Abbreviation ();
  void set_stem (Stem *);
};

#endif /* ABBREV_HH */

