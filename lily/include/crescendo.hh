/*
  crescendo.hh -- declare Crescendo

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "staff-side.hh"
#include "spanner.hh"
/**
  The hairpin symbol. (cresc)
 */
class Crescendo : public Spanner , public Staff_side {
public:
  int grow_dir_;
    
  /// if there is a dynamic at the end, make the sign smaller.
  Drul_array<bool> dyn_b_drul_;

  Crescendo();
protected:
  SCORE_ELEMENT_CLONE(Crescendo);
  virtual Molecule*brew_molecule_p() const;
  virtual Interval symbol_height() const;
  DECLARE_MY_RUNTIME_TYPEINFO;
    
private:
  Atom get_symbol() const;
};

#endif // CRESCENDO_HH
