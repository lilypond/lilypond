/*
  beam.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH
#include "lily-proto.hh"
#include "directional-spanner.hh"
#include "plist.hh"

/** a beam connects multiple stems Beam adjusts the stems its owns to
  make sure that they reach the beam and that point in the correct
  direction */
class Beam:  public Directional_spanner {
public:
    Link_array<Stem> stems;
    /// the slope of the beam in posns / point (dimension)   
    Real slope;

    /// position of leftmost end of beam  
    Real left_pos;
   

    /* *************** */
    NAME_MEMBERS();
    Beam();
    void add(Stem*);

    void set_grouping(Rhythmic_grouping def, Rhythmic_grouping current);
    void set_stemlens();
    SCORE_ELEM_CLONE(Beam);
protected:
    virtual Interval do_width()const;    
    virtual Offset center() const;
    virtual void set_default_dir();
    virtual void do_pre_processing();
    virtual void do_post_processing();
    virtual void do_substitute_dependency(Score_elem*, Score_elem*);

    virtual void do_print() const;

private:
    Molecule stem_beams(Stem *here, Stem *next, Stem *prev)const;
    void solve_slope();
    Molecule*brew_molecule_p()const;
};

#endif // BEAM_HH

