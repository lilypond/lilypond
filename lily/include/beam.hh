/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "directional-spanner.hh"
#include "stem-info.hh"


/** a beam connects multiple stems.

  Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction

elt property:

damping: amount of beam slope damping. (int)

should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams 


*/
class Beam : public Directional_spanner  {
public:
  /** 
    The beams should be prevented to conflict with the stafflines, 
    especially at small slopes.
    */
  enum Quantisation { NONE, NORMAL, TRADITIONAL };
  enum Dir_algorithm { /* DOWN=-1, UP=1, */ MAJORITY=2, MEAN, MEDIAN };

  Link_array<Stem> stems_;
  /**
     the slope of the beam in (staffpositions) per (X-dimension, in PT).
     UGH. standardise this for once and for all.
   */
  Real slope_f_;

  /// position of leftmost end of beam  
  Real left_y_;

  /** should beam pos / slope be quantised? 0: no, 1: yes, 2: traditional
      JUNKME.
   */
  Quantisation quantisation_;
  
  /// maximum number of beams (for opening-up of beam-spacing)
  int multiple_i_;

  Array<Stem_info> sinfo_;
  
  Beam();
  void add_stem (Stem*);
  Stem_info get_stem_info (Stem*);

  void set_grouping (Rhythmic_grouping def, Rhythmic_grouping current);
  void set_beaming (Beaming_info_list *);
  void set_stemlens ();
  VIRTUAL_COPY_CONS(Score_element);

protected:
  virtual Interval do_width () const;    
  Offset center () const;
  Direction get_default_dir () const;
  void set_direction (Direction);
  void set_steminfo ();
  bool auto_knee (SCM gap, bool interstaff_b);
  bool auto_knees ();
  
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
  virtual void do_substitute_element_pointer (Score_element*, Score_element*);
  virtual void do_add_processing ();
  virtual void do_print() const;
  virtual Molecule*do_brew_molecule_p () const;

  Molecule stem_beams (Stem *here, Stem *next, Stem *prev) const;

private:
  void calculate_slope ();
  Real check_stemlengths_f (bool set_b);
  void solve_slope ();

  void quantise_left_y (bool extend_b);
  void quantise_dy ();

};

#endif // BEAM_HH

