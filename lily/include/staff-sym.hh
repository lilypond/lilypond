/*
  staffsym.hh -- declare Staff_symbol

  source file of the GNU LilyPond music typesetter

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
    /// this many lines.
    int no_lines_i_;
public:


    void set_extent(PCol* p1, PCol* p2);
    NAME_MEMBERS();
    Staff_symbol(int lines);
    Real inter_note_f()const;
    int steps_i()const;
protected:
    SPANNER_CLONE(Staff_symbol)
    virtual Molecule* brew_molecule_p() const;
    virtual void do_print()const;
};
#endif // STAFFSYM_HH
