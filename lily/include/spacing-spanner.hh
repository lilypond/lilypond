/*
  spacing-spanner.hh -- declare Spacing spanner

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef SPACING_SPANNER_HH
#define SPACING_SPANNER_HH

#include "lily-proto.hh"
#include "lily-guile.hh"
#include "rational.hh"
#include "std-vector.hh"

struct Spacing_options
{
  bool packed_;
  bool stretch_uniformly_;
  bool float_nonmusical_columns_;
  bool float_grace_columns_;
  Rational global_shortest_;
  Real increment_;
  Real shortest_duration_space_;

  Spacing_options();
  void init_from_grob (Grob *me);
  Real get_duration_space (Moment d, bool *) const;
};

/*
  TODO: prune to public interface.
*/
class Spacing_spanner
{
public:
  static void generate_pair_spacing (Grob *me,
				     Paper_column *l, Paper_column *r,
				     Paper_column *nextr,
				     Spacing_options const *options);
  static void standard_breakable_column_spacing (Grob *me, Item *l, Item *r,
						 Real *fixed, Real *space,
						 Spacing_options const *);
  static Real default_bar_spacing (Grob *, Grob *, Grob *, Moment);
  static Real note_spacing (Grob *, Grob *, Grob *, Spacing_options const *, bool *);
  static Real get_duration_space (Moment dur, Spacing_options const *, bool *);
  static Rational find_shortest (Grob *, vector<Grob*> const &);
  static Rational effective_shortest_duration (Grob *me, vector<Grob*> const &all);
  static void breakable_column_spacing (Grob *, Item *l, Item *r, Spacing_options const *);
  static void prune_loose_columns (Grob *, vector<Grob*> *cols, Spacing_options const *);
  static void set_explicit_neighbor_columns (vector<Grob*> const &cols);
  static void set_implicit_neighbor_columns (vector<Grob*> const &cols);
  static void generate_springs (Grob *me, vector<Grob*> const &cols, Spacing_options const *);
  static void musical_column_spacing (Grob *, Item *, Item *, Spacing_options const *);
  DECLARE_SCHEME_CALLBACK (set_springs, (SCM));
  static bool has_interface (Grob *);
};

#endif /* SPACING_SPANNER_HH */
