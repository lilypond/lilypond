/*
  staffsym.hh -- declare Staff_symbol

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#ifndef STAFFSYM_HH
#define STAFFSYM_HH
#include "spanner.hh"

/**
  This spanner draws the lines of a pstaff.
  The bottom line is position 0.
  */
class Staff_symbol : public Spanner
{
public:
  /// this many lines.
  int no_lines_i_;
  Real staff_line_leading_f_;
  
  Staff_symbol ();
  int steps_i() const;
protected:
  VIRTUAL_COPY_CONS(Score_element);
  virtual Interval do_height () const;
  virtual Molecule* do_brew_molecule_p() const;
  virtual void do_print() const;
};
#endif // STAFFSYM_HH
