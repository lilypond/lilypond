/*
  crescendo.hh -- declare Crescendo

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef CRESCENDO_HH
#define CRESCENDO_HH

#include "staff-side.hh"
#include "spanner.hh"
/**
  The hairpin symbol. (cresc)
 */
class Crescendo : public Spanner , public Staff_side {
public:
    int grow_dir_i_;
    
/// if there is a dynamic at the end, make the sign smaller.
    bool right_dyn_b_;

    /// if there is a dynamic at the end, make the sign smaller.
    bool left_dyn_b_;
    Crescendo();
protected:
    SCORE_ELEM_CLONE(Crescendo);
    virtual Molecule*brew_molecule_p()const;
    NAME_MEMBERS();
    
private:
    
};

#endif // CRESCENDO_HH
