/*   
  abbrev.hh -- declare Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
  
 */

#ifndef ABBREV_HH
#define ABBREV_HH

#include "item.hh"
class Abbreviation : public Item {
  Stem * stem_l_;
protected:
  virtual void do_print () const;
  virtual Molecule *brew_molecule_p () const;
  virtual void do_substitute_dependent (Score_elem*, Score_elem*);
public:
  int abbrev_flags_i_;
  Abbreviation ();
  void set_stem (Stem *);
};

#endif /* ABBREV_HH */

