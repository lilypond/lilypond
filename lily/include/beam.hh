/*
  beam.hh -- part of GNU LilyPond

  (c) 1996--2002 Han-Wen Nienhuys
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stem-info.hh"


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
  static Real get_interbeam (Grob*me);
  DECLARE_SCHEME_CALLBACK (space_function, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (brew_molecule, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));

  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (least_squares, (SCM));
  DECLARE_SCHEME_CALLBACK (check_concave, (SCM));
  DECLARE_SCHEME_CALLBACK (slope_damping, (SCM));
  DECLARE_SCHEME_CALLBACK (shift_region_to_valid, (SCM));  
  DECLARE_SCHEME_CALLBACK (quanting, (SCM));
  static Real score_slopes_dy (Grob*, Real,Real,Real,Real);
  static Real score_stem_lengths (Link_array<Grob>,
				  Array<Stem_info>,
				  Array<Real>, Array<Real>, 
				  bool,Grob*,Real , Real);
  static Real score_forbidden_quants (Grob*, Real, Real,
				      Real, Real, Real, Real,
				      int, Direction, Direction);
  
  
  static Molecule stem_beams (Grob*,Item *here, Item *next, Item *prev,
			      Real dydx);

private:
  static Direction get_default_dir (Grob*);
  static void set_stem_directions (Grob*, Direction );
  static void consider_auto_knees (Grob*, Direction d);
  static void set_stem_shorten (Grob*);
  static Real calc_stem_y (Grob*, Grob* s, Interval pos, bool correct);
  static void set_stem_lengths (Grob*);
  static int forced_stem_count (Grob*);
};

#endif /* BEAM_HH */

