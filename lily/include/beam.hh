/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"



class Beam
{
public:
  static int visible_stem_count (Grob*);
  static Item* first_visible_stem (Grob*);
  static Item* last_visible_stem (Grob*);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (rest_collision_callback, (SCM element, SCM axis));
  Beam (SCM);
  static void add_stem (Grob*,Grob*);
  static void set_beaming (Grob*,Beaming_info_list *);
  static void set_stemlens (Grob*);
  static int get_multiplicity (Grob*me);
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM ));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM ));
  DECLARE_SCHEME_CALLBACK (end_after_line_breaking, (SCM));

  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (least_squares, (SCM));
  DECLARE_SCHEME_CALLBACK (check_concave, (SCM));
  DECLARE_SCHEME_CALLBACK (slope_damping, (SCM));
  DECLARE_SCHEME_CALLBACK (quantise_position, (SCM));
  DECLARE_SCHEME_CALLBACK (new_quanting, (SCM));
  DECLARE_SCHEME_CALLBACK (score_slopes_dy, (SCM, SCM,SCM));
  DECLARE_SCHEME_CALLBACK (score_stem_lengths, (SCM, SCM,SCM));
  DECLARE_SCHEME_CALLBACK (score_forbidden_quants, (SCM, SCM,SCM));
  
  
  static Molecule stem_beams (Grob*,Item *here, Item *next, Item *prev,
			      Real dydx);

private:
  static Direction get_default_dir (Grob*);
  static void set_stem_directions (Grob*);
  static void consider_auto_knees (Grob*);
  static void set_stem_shorten (Grob*);
  static Real calc_stem_y_f (Grob*, Item* s, Interval pos);
  static Real check_stem_length_f (Grob*, Interval pos);
  static void set_stem_lengths (Grob*);
  static Interval quantise_interval (Grob*, Interval pos, Direction quant_dir);
  static int forced_stem_count (Grob*);
};

#endif /* BEAM_HH */

