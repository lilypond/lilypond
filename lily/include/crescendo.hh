/*
  crescendo.hh -- declare Crescendo

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "spanner.hh"

class Crescendo : public Spanner {
    int staff_size_i_;
public:
    int grow_dir_i_;
    int dir_i_;
    Crescendo(int staff_size_i);
private:
    Spanner* do_break_at( PCol*, PCol*) const;
    Molecule*brew_molecule_p()const;
    NAME_MEMBERS(Crescendo);
};

#endif // CRESCENDO_HH
