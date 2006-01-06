/*
  beam.hh -- part of GNU LilyPond

  source file of the LilyPond music typesetter

  (c) 1996--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#ifndef BEAM_HH
#define BEAM_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "stem-info.hh"

/*
  TODO: move quanting in separate file.
*/
struct Beam_quant_parameters
{
  Real INTER_QUANT_PENALTY;
  Real SECONDARY_BEAM_DEMERIT;
  Real STEM_LENGTH_DEMERIT_FACTOR;
  Real REGION_SIZE;

  /*
    threshold to combat rounding errors.
  */
  Real BEAM_EPS;

  // possibly ridiculous, but too short stems just won't do
  Real STEM_LENGTH_LIMIT_PENALTY;
  Real DAMPING_DIRECTION_PENALTY;
  Real MUSICAL_DIRECTION_FACTOR;
  Real IDEAL_SLOPE_FACTOR;
  Real ROUND_TO_ZERO_SLOPE;

  void fill (Grob *him);
};

class Beam
{
public:
  static int visible_stem_count (Grob *);
  static Grob *first_visible_stem (Grob *);
  static Grob *last_visible_stem (Grob *);
  static bool has_interface (Grob *);
  static void add_stem (Grob *, Grob *);
  static bool is_knee (Grob *);
  static void set_beaming (Grob *, Beaming_info_list const *);
  static void set_stemlens (Grob *);
  static int get_beam_count (Grob *me);
  static Real get_beam_translation (Grob *me);
  static Real get_thickness (Grob *me);
  static void connect_beams (Grob *me);

  DECLARE_SCHEME_CALLBACK (rest_collision_callback, (SCM element, SCM prev_off));
  DECLARE_SCHEME_CALLBACK (print, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_beaming, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_stem_shorten, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_direction, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_positions, (SCM));
  DECLARE_SCHEME_CALLBACK (calc_least_squares_positions, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (calc_concaveness, (SCM));
  DECLARE_SCHEME_CALLBACK (set_stem_lengths, (SCM));

  /* position callbacks */
  DECLARE_SCHEME_CALLBACK (shift_region_to_valid, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (slope_damping, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK (quanting, (SCM, SCM));
  
static Real score_slopes_dy (Real, Real, Real, Real, Real, bool, Beam_quant_parameters const *);

  static Real score_stem_lengths (Link_array<Grob> const &stems,
				  Array<Stem_info> const &stem_infos,
				  Array<Real> const &base_stem_ys,
				  Array<Real> const &stem_xs,
				  Real xl, Real xr,
				  bool knee,
				  Real yl, Real yr, Beam_quant_parameters const *);
  static Real score_forbidden_quants (Real, Real,
				      Real, Real, Real, Real,
				      Drul_array<int>, Direction, Direction,
				      Beam_quant_parameters const *);

  static int get_direction_beam_count (Grob *me, Direction d);
private:
  static Direction get_default_dir (Grob *);
  static void set_stem_directions (Grob *, Direction);
  static void consider_auto_knees (Grob *);
  static void set_stem_shorten (Grob *);
  static Real calc_stem_y (Grob *, Grob *s, Grob **c,
			   Real, Real,
			   Drul_array<Real> pos, bool french);
  static int forced_stem_count (Grob *);
};

#ifndef NDEBUG
#define DEBUG_QUANTING 1
#endif

#endif /* BEAM_HH */

