/*
  bow.hh -- declare Bow

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef BOW_HH
#define BOW_HH
#include "directional-spanner.hh"

/**
  base class for anything that looks like a slur.
  */
class Bow : public Directional_spanner {
protected:
    int left_pos_i_;
    int right_pos_i_;
    Real left_dx_f_;
    Real right_dx_f_;
    Molecule*brew_molecule_p()const;
public:
    Bow();
    NAME_MEMBERS(Bow);
    Offset center() const;  
};
#endif // BOW_HH
