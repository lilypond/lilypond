/*
  beam.hh -- part of LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH
#include "proto.hh"
#include "directionalspanner.hh"
#include "plist.hh"

/** a beam connects multiple stems Beam adjusts the stems its owns to
make sure that they reach the beam and that point in the correct
direction */
struct Beam:  public Directional_spanner {
    PointerList<Stem*> stems;
    Real slope;
    Real left_pos;
   

    /* *************** */
    const char * name() const;
    
    virtual Interval width()const;    
    Offset center() const;
    Spanner *do_break_at(PCol *,  PCol *) const;
    Beam();
    void add(Stem*);
    

    void set_default_dir();
    void do_pre_processing();
    void do_post_processing();

    void do_print() const;
    void set_grouping(Rhythmic_grouping def, Rhythmic_grouping current);
    void set_stemlens();
    ~Beam();

private:
    Molecule stem_beams(Stem *here, Stem *next, Stem *prev)const;
    void solve_slope();
    Molecule*brew_molecule_p()const;
};

#endif // BEAM_HH

