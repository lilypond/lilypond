/*
  bow.hh -- declare Bow

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BOW_HH
#define BOW_HH
#include "directional-spanner.hh"

/**
  Base class for anything that looks like a slur.
  Anybody with a better name?
  */
class Bow : public Directional_spanner {
protected:
  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;

  virtual Real height_f () const;
  virtual Molecule* brew_molecule_p () const;
public:
  Bow();
  DECLARE_MY_RUNTIME_TYPEINFO;
  Offset center() const;  
};
#endif // BOW_HH
