/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2022 Han-Wen Nienhuys

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef GROB_HH
#define GROB_HH

#include "box.hh"
#include "virtual-methods.hh"
#include "diagnostics.hh"
#include "dimension-cache.hh"
#include "grob-interface.hh"
#include "lily-proto.hh"

#include <set>
#include <vector>

typedef Interval_t<vsize> System_rank_interval;

class Grob : public Smob<Grob>, public Diagnostics
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Grob ();
  VIRTUAL_CLASS_NAME (Grob);

private:
  void init ();

private:
  /* data */
  mutable Dimension_cache dim_cache_[NO_AXES];
  Output_def *layout_;
  Grob *original_;

  /* SCM data */
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  SCM object_alist_;

  /* centralized GC marking: all Grobs from the same system share this pool. */
  SCM protection_pool_;

  /*
    If this is a property, it accounts for 25% of the property
    lookups.
  */
  SCM interfaces_;

protected:
  void add_interface (SCM sym) { interfaces_ = scm_cons (sym, interfaces_); }
  void set_layout (Output_def *layout) { layout_ = layout; }
  void substitute_object_links (Direction, SCM);
  void substitute_object_links (System *, SCM);
  Real get_offset (Axis a) const;
  SCM try_callback (SCM, SCM);
  SCM try_callback_on_alist (SCM *, SCM, SCM);
  void internal_set_value_on_alist (SCM *alist, SCM sym, SCM val);

  /* messages */
  Input *origin () const override;

public:
  /* friends */
  friend class System;
  friend SCM ly_grob_properties (SCM);
  friend SCM ly_grob_basic_properties (SCM);

  /* standard callbacks */
  DECLARE_SCHEME_CALLBACK (x_parent_positioning, (SCM));
  DECLARE_SCHEME_CALLBACK (y_parent_positioning, (SCM));
  DECLARE_SCHEME_CALLBACK (pure_stencil_height, (SCM smob, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (stencil_height, (SCM smob));
  DECLARE_SCHEME_CALLBACK (stencil_width, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_simple_vertical_skylines_from_extents,
                           (SCM smob, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (simple_vertical_skylines_from_extents, (SCM smob));
  DECLARE_SCHEME_CALLBACK (vertical_skylines_from_stencil, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_vertical_skylines_from_element_stencils,
                           (SCM smob, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (vertical_skylines_from_element_stencils, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_simple_horizontal_skylines_from_extents,
                           (SCM smob, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (simple_horizontal_skylines_from_extents, (SCM smob));
  DECLARE_SCHEME_CALLBACK (horizontal_skylines_from_stencil, (SCM smob));
  DECLARE_SCHEME_CALLBACK (pure_horizontal_skylines_from_element_stencils,
                           (SCM smob, SCM, SCM));
  DECLARE_SCHEME_CALLBACK (horizontal_skylines_from_element_stencils,
                           (SCM smob));

  /* R/O access */
  Output_def *layout () const { return layout_; }
  Grob *original () const { return original_; }
  SCM interfaces () const { return interfaces_; }

  /* life & death */
  Grob (SCM basic_props);
  Grob (Grob const &);
  virtual Grob *clone () const = 0;
  virtual Grob *make_sticky_same_type (Engraver *eng, SCM type, SCM cause,
                                       char const *file, int line,
                                       char const *fun)
    = 0;

  /* forced death */
  void suicide ();
  bool is_live () const;

  /* naming. */
  std::string name () const;

  /* Properties */
  SCM get_property_alist_chain (SCM) const;
  SCM internal_get_property (SCM symbol) const;
  SCM internal_get_property_data (SCM symbol) const;
  SCM internal_get_pure_property (SCM symbol, vsize start, vsize end) const;
  SCM internal_get_maybe_pure_property (SCM symbol, bool pure, vsize start,
                                        vsize end) const;
  SCM internal_get_non_callback_marker_property_data (SCM symbol) const;
  SCM internal_get_object (SCM symbol) const;
  void internal_set_object (SCM sym, SCM val);
  void internal_del_property (SCM symbol);
  void instrumented_set_property (SCM, SCM, char const *, int, char const *);
  void internal_set_property (SCM sym, SCM val);

  /* causes */
  Stream_event *event_cause () const;
  Stream_event *ultimate_event_cause () const;

  /* class hierarchy */
  virtual System *get_system () const;
  static System *get_system (Grob *);
  virtual void do_break_processing ();
  virtual Grob *find_broken_piece (System *) const;
  virtual void break_breakable_item (System *);
  virtual void derived_mark () const;
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();
  virtual bool internal_set_as_bound_of_spanner (Spanner *, Direction)
  {
    return false;
  }

  // get the relevant piece of this grob in the context of a line running from
  // `start` to `end` -- nullptr if the grob would not be visible
  virtual Grob *pure_find_visible_prebroken_piece (vsize start,
                                                   vsize end) const = 0;

  /* printing */
  const Stencil *get_stencil () const;
  Stencil get_print_stencil () const;

  /* interfaces */
  bool internal_has_interface (SCM intf) const;

  /* offsets */
  void translate_axis (Real, Axis);
  Real relative_coordinate (Grob const *refp, Axis) const;
  Real parent_relative (Grob const *refp, Axis) const;
  Real pure_relative_y_coordinate (Grob const *refp, vsize start, vsize end);
  Real maybe_pure_coordinate (Grob const *refp, Axis a, bool pure, vsize start,
                              vsize end);

  /* extents */
  Interval extent (Grob const *refpoint, Axis) const;
  void flush_extent_cache (Axis);
  virtual Interval pure_y_extent (Grob *refpoint, vsize start, vsize end);
  Interval maybe_pure_extent (Grob *refpoint, Axis, bool pure, vsize start,
                              vsize end);

  /* refpoints */
  Grob *common_refpoint (Grob const *s, Axis a) const;
  void set_x_parent (Grob *e) { dim_cache_[X_AXIS].parent_ = e; }
  void set_y_parent (Grob *e) { dim_cache_[Y_AXIS].parent_ = e; }
  void set_parent (Grob *e, Axis a) { dim_cache_[a].parent_ = e; }
  Grob *get_x_parent () const { return dim_cache_[X_AXIS].parent_; }
  Grob *get_y_parent () const { return dim_cache_[Y_AXIS].parent_; }
  Grob *get_parent (Axis a) const { return dim_cache_[a].parent_; }
  void fixup_refpoint ();
  bool has_in_ancestry (const Grob *possible_ancestor, Axis a) const;

  /* vertical ordering */
  static bool internal_vertical_less (Grob *g1, Grob *g2, bool pure);
  static Grob *get_root_vertical_alignment (Grob *g);
  static Grob *get_vertical_axis_group (Grob *g);
  static bool vertical_less (Grob *g1, Grob *g2);
  static bool pure_vertical_less (Grob *g1, Grob *g2);
  static int get_vertical_axis_group_index (Grob *g);

  /* skylines */
  virtual Interval_t<int> spanned_column_rank_interval () const = 0;
  virtual System_rank_interval spanned_system_rank_interval () const = 0;
  bool check_cross_staff (Grob *common);
  static bool less (Grob *g1, Grob *g2);
  static SCM maybe_pure_internal_simple_skylines_from_extents (Grob *, Axis,
                                                               bool, int, int,
                                                               bool, bool);
  static SCM internal_skylines_from_element_stencils (Grob *me, Axis a,
                                                      bool pure, int beg,
                                                      int end);
  static SCM internal_skylines_from_element_stencils (SCM, Axis);
};

template <class T>
inline bool
has_interface (Grob const *g)
{
  return g && g->internal_has_interface (Grob_interface<T>::interface_symbol_);
}

/* unification */
void uniquify (std::vector<Grob *> &);

/* refpoints */
Grob *common_refpoint_of_list (SCM elt_list, Grob *, Axis a);
Grob *common_refpoint_of_array (std::vector<Grob *> const &, Grob *, Axis a);
Grob *common_refpoint_of_array (std::set<Grob *> const &, Grob *, Axis a);
System *get_root_system (Grob *me);

/* extents */
Interval robust_relative_extent (Grob *, Grob *, Axis);
Interval robust_relative_pure_y_extent (Grob *, Grob *, vsize, vsize);

/* offset/extent callbacks. */
void add_offset_callback (Grob *g, SCM proc, Axis a);
void chain_offset_callback (Grob *g, SCM proc, Axis a);
void chain_callback (Grob *g, SCM proc, SCM sym);
SCM axis_offset_symbol (Axis a);
SCM axis_parent_positioning (Axis a);

SCM call_pure_function (SCM unpure, SCM args, vsize start, vsize end);

void set_nested_property (Grob *, SCM property_path, SCM value);

SCM check_debug_callback (SCM cb);

#endif /* GROB_HH */
