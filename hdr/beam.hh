/*
  beam.hh -- part of LilyPond

  (c) 1996 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH
#include "proto.hh"
#include "directionalspanner.hh"
#include "plist.hh"

/// a beam connects multiple stems 
struct Beam:  public Directional_spanner {
    PointerList<Stem*> stems;
    Real slope;
    Real left_pos;
    /// dir: -1 below heads, +1 above heads.


    Rhythmic_grouping *group;

    /****************/
    
    virtual Interval width()const;    
    Offset center() const;
    Spanner *broken_at(PCol *,  PCol *) const;
    Beam();
    void add(Stem*);
    void process();
    void calculate();
    void set_default_dir();
    void preprocess();
    Interval height()const;
    void print() const;
    void set_grouping(Rhythmic_grouping def, Rhythmic_grouping current);
    void set_stemlens();
    ~Beam();
private:
    Molecule stem_beams(Stem *here, Stem *next, Stem *prev);
    void solve_slope();
    void brew_molecule();
};
/** Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction */

#endif // BEAM_HH

