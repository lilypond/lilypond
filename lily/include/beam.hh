/*
  beam.hh -- part of GNU LilyPond

  source file of the LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
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
  static Grob* first_visible_stem (Grob*);
  static Grob* last_visible_stem (Grob*);
  static bool has_interface (Grob*);
  DECLARE_SCHEME_CALLBACK (rest_collision_callback, (SCM element, SCM axis));
  Beam (SCM);
  static void add_stem (Grob*,Grob*);
  static bool is_knee (Grob*);
  static void set_beaming (Grob*,Beaming_info_list *);
  static void set_stemlens (Grob*);
  static int get_beam_count (Grob*me);
  static void position_beam (Grob* me);
  static Real get_beam_translation (Grob*me);
  static Real get_thickness (Grob*me);

  static void connect_beams (Grob*me);
  DECLARE_SCHEME_CALLBACK (space_function, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (before_line_breaking, (SCM));
  DECLARE_SCHEME_CALLBACK (after_line_breaking, (SCM));
  
  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (least_squares, (SCM));
  DECLARE_SCHEME_CALLBACK (check_concave, (SCM));
  DECLARE_SCHEME_CALLBACK (slope_damping, (SCM));
  DECLARE_SCHEME_CALLBACK (shift_region_to_valid, (SCM));  
  DECLARE_SCHEME_CALLBACK (quanting, (SCM));
  static Real score_slopes_dy (Real, Real, Real, Real, Real, bool);

  static Real score_stem_lengths (Link_array<Grob> const &stems,
				  Array<Stem_info> const &stem_infos,
				  Array<Real> const &base_stem_ys,
				  Array<Real> const &stem_xs,
				  Real xl, Real xr, 
				  bool knee, 
				  Real yl, Real yr);
  static Real score_forbidden_quants (Real, Real,
				      Real, Real, Real, Real,
				      Drul_array<int>, Direction, Direction);
  

  static int get_direction_beam_count (Grob *me, Direction d);
private:
  static Direction get_default_dir (Grob*);
  static void set_stem_directions (Grob*, Direction );
  static void consider_auto_knees (Grob*);
  static void set_stem_shorten (Grob*);
  static Real calc_stem_y (Grob*, Grob* s, Grob**c,
			   Real, Real,
			   Drul_array<Real> pos, bool french);
  static void set_stem_lengths (Grob*);
  static int forced_stem_count (Grob*);
};

const int REGION_SIZE = 2;

#ifndef NDEBUG
#define DEBUG_QUANTING 1
#endif

#endif /* BEAM_HH */

