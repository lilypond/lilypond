/*
  bow.hh -- declare Bow

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef BOW_HH
#define BOW_HH

#include "directional-spanner.hh"
#include "curve.hh"

/**
  Base class for anything that looks like a slur.
  Anybody with a better name?
  */
class Bow : public Directional_spanner
{
public:
  Bow ();
  Offset center () const;  

  

  int dash_i_;
  Real interstaff_f_;
  Drul_array<Real> vertical_align_drul_;

protected:
  virtual Molecule* do_brew_molecule_p () const;
  virtual Interval do_width () const;    
  Array<Offset> get_controls () const;
  virtual Array<Offset> get_encompass_offset_arr () const;
  virtual Interval do_height () const;

  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;
};

#endif // BOW_HH
