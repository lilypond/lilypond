/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "spanner.hh"


/** a beam connects multiple stems.

  Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction (urg?)

   elt_properties:
   y-position: real  (position of left edge)
   height: real  (dy)

   damping: amount of beam slope damping. (int)
   should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams 
*/
class Beam : public Spanner
{
public:

  int stem_count () const;
  Stem* stem (int) const;
  Stem* stem_top () const;
  int visible_stem_count () const;
  Stem* first_visible_stem () const;
  Stem* last_visible_stem () const;

  Beam (SCM);
  void add_stem (Stem*);
  void set_grouping (Rhythmic_grouping def, Rhythmic_grouping current);
  void set_beaming (Beaming_info_list *);
  void set_stemlens ();
  VIRTUAL_COPY_CONS(Score_element);

  int get_multiplicity () const;

protected:
 
  virtual void before_line_breaking ();
  virtual void after_line_breaking ();
  virtual Molecule do_brew_molecule () const;

  Molecule stem_beams (Stem *here, Stem *next, Stem *prev) const;
private:
  Direction get_default_dir () const;
  void set_stem_directions ();
  void auto_knees ();
  bool auto_knee (String gap_str, bool interstaff_b);
  void set_stem_shorten ();
  void calc_default_position_and_height (Real* y, Real* dy) const;
  bool suspect_slope_b (Real y, Real dy) const;
  Real calc_slope_damping_f (Real dy) const;
  Real calc_stem_y_f (Stem* s, Real y, Real dy) const;
  Real check_stem_length_f (Real y, Real dy) const;
  void set_stem_length (Real y, Real dy);
  Real quantise_dy_f (Real dy) const;
  Real quantise_y_f (Real y, Real dy, int quant_dir);
  int forced_stem_count () const;
};

#endif // BEAM_HH

