/*
  staffsym.hh -- declare Staff_symbol

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
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

    const char *name()const;
    Staff_symbol(int lines);
    virtual Molecule* brew_molecule_p() const;
    void set_extent(PCol* p1, PCol* p2);
    virtual void do_print()const;
    virtual Spanner *do_break_at( PCol *c1,  PCol *c2) const;
};
#endif // STAFFSYM_HH
