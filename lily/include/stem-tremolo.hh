/*   
  stem-tremolo.hh -- declare Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ABBREV_HH
#define ABBREV_HH

#include "item.hh"


class Stem_tremolo : public Item {
  Stem * stem_l_;
protected:
  virtual void do_print () const;
  virtual Molecule *do_brew_molecule_p () const;
  virtual void do_substitute_element_pointer (Score_element*, Score_element*);
  virtual Interval do_width () const;
  virtual void do_pre_processing ();
public:
  int abbrev_flags_i_;
  Stem_tremolo ();
  void set_stem (Stem *);
};

#endif /* ABBREV_HH */

