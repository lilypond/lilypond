/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--2000 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"


/** a beam connects multiple stems.

  Beam adjusts the stems its owns to make sure that they reach the
  beam and that point in the correct direction (urg?)

   elt_properties:
   
   y-position -- real  (position of left edge)

   height -- real  (dy)


   Read-only
   =========
   
   flag-width-function --

   damping -- amount of beam slope damping. (int)
     should beam slope be damped? 0: no, 1: yes, 100000: horizontal beams 

   default-neutral-direction -- which direction to choose if we're in
     the middle of the staff
   
   thickness -- weight of beams, in staffspace

   space-function -- function of type multiplicity -> real (in staffspace)

   beamed-stem-shorten --

   height-quants --

   vertical-position-quant-function --

   dir-function --
   
   damping -- damping factor (real).

   outer-stem-length-limit -- catch suspect beam slopes, set slope to zero if
     outer stem is lengthened more than this (in staffspace)

   slope-limit -- set slope to zero if slope is running away steeper than this.
*/
class Beam
{
public:
  static int visible_stem_count (Score_element*);
  static  Item* first_visible_stem (Score_element*);
  static  Item* last_visible_stem (Score_element*);
  static bool has_interface (Score_element*);
  static void set_interface (Score_element*);  
  static Real rest_collision_callback (Score_element *,Axis);
  Beam (SCM);
  static void add_stem (Score_element*,Score_element*);
  static void set_beaming (Score_element*,Beaming_info_list *);
  static void set_stemlens (Score_element*);
  static int get_multiplicity (Score_element*me);
  static SCM brew_molecule (SCM);
  static SCM before_line_breaking (SCM);
  static SCM after_line_breaking (SCM);
  static Molecule stem_beams (Score_element*,Item *here, Item *next, Item *prev);

private:
  static Direction get_default_dir (Score_element*);
  static  void set_stem_directions (Score_element*);
  static  void auto_knees (Score_element*);
  static  bool auto_knee (Score_element*,String gap_str, bool interstaff_b);
  static void set_stem_shorten (Score_element*);
  static  void calc_default_position_and_height (Score_element*,Real* y, Real* dy);
  static  bool suspect_slope_b (Score_element*, Real y, Real dy);
  static  Real calc_slope_damping_f (Score_element*, Real dy);
  static  Real calc_stem_y_f (Score_element*, Item* s, Real y, Real dy);
  static  Real check_stem_length_f (Score_element*, Real y, Real dy);
  static  void set_stem_length (Score_element*, Real y, Real dy);
  static   Real quantise_dy_f (Score_element*, Real dy);
  static  Real quantise_y_f (Score_element*, Real y, Real dy, int quant_dir);
  static int forced_stem_count (Score_element*);
};

#endif // BEAM_HH

