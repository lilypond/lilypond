/*
  tie.hh -- declare Tie

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef TIE_HH
#define TIE_HH

#include "spanner.hh"
#include "rod.hh"

/**
  Connect two noteheads.
  */
class Tie : public Spanner
{
public:
  Tie (SCM);
  void set_head (Direction, Item*head_l);
  VIRTUAL_COPY_CONS(Score_element);

  Note_head* head (Direction) const;
  Real position_f () const;
  
  virtual Direction get_default_dir() const;

protected:
  virtual Molecule do_brew_molecule () const;
  virtual Array<Offset> get_encompass_offset_arr () const;
  Bezier get_curve () const;

  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;

  virtual void do_add_processing ();
  virtual void after_line_breaking ();

  virtual Array<Rod> get_rods () const;

  Array<Offset> get_controls () const;
};

#endif // TIE_HH
