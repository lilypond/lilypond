/*
  crescendo.hh -- declare Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "spanner.hh"
/**
  The hairpin symbol. (cresc)

  (normal spanner?)
 */
class Crescendo : public Spanner {
public:
  Crescendo();
protected:
  VIRTUAL_COPY_CONS(Score_element);
  virtual Molecule*do_brew_molecule_p() const;
    
private:
  Molecule get_symbol() const;
};

#endif // CRESCENDO_HH
