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

  Rhythmic_head* head (Direction) const;
  Real position_f () const;
  static SCM brew_molecule (SCM);
  Direction get_default_dir() const;
  SCM member_brew_molecule () const;
  Array<Offset> get_encompass_offset_arr () const;
  Bezier get_curve () const;

  /*
    JUNKME
   */
  Drul_array<Real> dy_f_drul_;
  Drul_array<Real> dx_f_drul_;

  virtual void do_add_processing ();
  SCM member_after_line_breaking ();
  static SCM after_line_breaking (SCM);

  virtual Array<Rod> get_rods () const;

  Array<Offset> get_controls () const;
};

#endif // TIE_HH
