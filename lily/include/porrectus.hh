/*
  porrectus.hh

  Copyright (C) 2001 Juergen Reuter

  written for the GNU LilyPond music typesetter
*/

#ifndef PORRECTUS_HH
#define PORRECTUS_HH

#include "lily-guile.hh"

/*
  porrectus ligature
*/
class Porrectus
{
public:
  static void set_left_head (Grob *, Item *);
  static Item *get_left_head (Grob *);
  static void set_right_head (Grob *, Item *);
  static Item *get_right_head (Grob *);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));

private:
  static Molecule brew_vaticana_molecule (Item *, Real,
					  bool, Real, Real,
					  bool, Direction);
  static Molecule brew_mensural_molecule (Item *, Real,
					  bool, Real, Real,
					  bool, Direction);
  static Molecule brew_bezier_sandwich (Bezier, Bezier);
  static Molecule brew_horizontal_slope (Real, Real, Real);
  static Molecule create_ledger_line (Interval, Grob *);
  static Molecule create_streepjes (Grob *, int, int, Interval);
};

#endif // PORRECTUS_HH
