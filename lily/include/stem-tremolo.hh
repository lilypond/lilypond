/*   
  stem-tremolo.hh -- declare Abbreviation
  
  source file of the GNU LilyPond music typesetter
  
  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#ifndef ABBREV_HH
#define ABBREV_HH

#include "item.hh"


class Stem_tremolo : public Item {
protected:
  Stem * stem_l () const;
  Molecule do_brew_molecule () const;

  static Interval dim_callback (Score_element*, Axis);
public:
   static SCM scheme_molecule (SCM);
  
Stem_tremolo (SCM);
  void set_stem (Stem *);
};

#endif /* ABBREV_HH */

