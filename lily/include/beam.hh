/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--1999 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "directional-spanner.hh"


/** a beam connects multiple stems.

  Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction

elt property:

damping: amount of beam slope damping. (int)

should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams 

slope_quantisation: 'none, 'normal or 'traditional

*/
class Beam : public Directional_spanner  {
public:

  int stem_count () const;
  Stem* stem (int) const;
  Stem* stem_top () const;
  int visible_stem_count () const;
  Stem* first_visible_stem () const;
  Stem* last_visible_stem () const;

  /**
     the slope of the beam in (staffpositions) per (X-dimension, in PT).
     UGH. standardise this for once and for all.
   */
  Real slope_f_;

  /// position of leftmost end of beam  
  Real left_y_;

  /** 
    highest number of beams present, for opening-up of beam-spacing
    and calculation of stem lengths
   */
  int multiplicity_i_;

  Beam ();
  void add_stem (Stem*);

  void set_grouping (Rhythmic_grouping def, Rhythmic_grouping current);
  void set_beaming (Beaming_info_list *);
  void set_stemlens ();
  VIRTUAL_COPY_CONS(Score_element);

protected:
  Offset center () const;
  Direction get_default_dir () const;
  void set_direction (Direction);
  void set_stem_shorten ();
  bool auto_knee (SCM gap, bool interstaff_b);
  bool auto_knees ();
  
  virtual void do_pre_processing ();
  virtual void do_post_processing ();
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

