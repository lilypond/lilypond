/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--1998 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH
#include "lily-proto.hh"
#include "directional-spanner.hh"
#include "plist.hh"

/** a beam connects multiple stems.

  Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction */
class Beam:  public Directional_spanner {
public:
  /** 
    The beams should be prevented to conflict with the stafflines, 
    especially at small slopes.
    */
  enum Quantisation { NONE, NORMAL, TRADITIONAL, TEST };
  enum Dir_algorithm { /* DOWN=-1, UP=1, */ MAJORITY=2, MEAN, MEDIAN };

  Link_array<Stem> stems_;
  /// the slope of the beam in posns / point (dimension)   
  Real slope_f_;
  /// the slope as solved; not quantised or damped
  Real solved_slope_f_;

  /// position of leftmost end of beam  
  Real left_y_;
  /// should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams
  int damping_i_;
  /// should beam pos / slope be quantised? 0: no, 1: yes, 2: traditional
  Quantisation quantisation_;
  /// maximum number of beams (for opening-up of beam-spacing)
  int multiple_i_;


  
  Beam();
  void add_stem (Stem*);

  void set_grouping (Rhythmic_grouping def, Rhythmic_grouping current);
  void set_stemlens ();
  VIRTUAL_COPY_CONS(Score_element);

protected:
  virtual Interval do_width () const;    
  Offset center () const;
  void set_default_dir ();
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  virtual void do_substitute_dependent (Score_element*, Score_element*);

  virtual void do_print() const;

  virtual void quantise_left_y (bool extend_b);
  virtual Molecule stem_beams (Stem *here, Stem *next, Stem *prev) const;
  virtual void solve_slope ();
  virtual void quantise_dy ();
  virtual Molecule*do_brew_molecule_p () const;
};

#endif // BEAM_HH

