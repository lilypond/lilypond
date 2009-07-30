/*
  grob.hh -- declare Grob

  source file of the LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys
*/

#ifndef GROB_HH
#define GROB_HH

#include "box.hh"
#include "virtual-methods.hh"
#include "dimension-cache.hh"
#include "grob-interface.hh"

class Grob
{
private:
  DECLARE_SMOBS (Grob);
  DECLARE_CLASSNAME(Grob);
  
  void init ();

protected:
  /* data */
  Dimension_cache dim_cache_[NO_AXES];
  Output_def *layout_;
  Grob *original_;

  /* SCM data */
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  SCM object_alist_;
  
  /*
    If this is a property, it accounts for 25% of the property
    lookups.
  */
  SCM interfaces_;
  
  void substitute_object_links (SCM, SCM);
  Real get_offset (Axis a) const;
  SCM try_callback (SCM, SCM);
  SCM try_callback_on_alist (SCM *, SCM, SCM);
  void internal_set_value_on_alist (SCM *alist, SCM sym, SCM val);

public:
  
  /* friends */
  friend class Spanner;
  friend class System;
  friend SCM ly_grob_properties (SCM);
  friend SCM ly_grob_basic_properties (SCM);

  /* standard callbacks */
  DECLARE_SCHEME_CALLBACK(x_parent_positioning, (SCM));
  DECLARE_SCHEME_CALLBACK(y_parent_positioning, (SCM));
  DECLARE_SCHEME_CALLBACK (stencil_height, (SCM smob));
  DECLARE_SCHEME_CALLBACK (stencil_width, (SCM smob));

  /* R/O access */
  Output_def *layout () const { return layout_; }
  Grob *original () const { return original_; }
  SCM interfaces () const { return interfaces_; }

  /* life & death */ 
  Grob (SCM basic_props);
  Grob (Grob const &);
  virtual Grob *clone () const;

  /* forced death */
  void suicide ();
  bool is_live () const;

  /* naming. */
  string name () const;

  /* Properties */
  SCM get_property_alist_chain (SCM) const;
  SCM internal_get_property (SCM symbol) const;
  SCM internal_get_property_data (SCM symbol) const;
  SCM internal_get_non_callback_marker_property_data (SCM symbol) const;
  SCM internal_get_object (SCM symbol) const;
  void internal_set_object (SCM sym, SCM val);
  void internal_del_property (SCM symbol);
  void instrumented_set_property (SCM, SCM, char const*, int, char const*);
  void internal_set_property (SCM sym, SCM val);

  /* messages */  
  void warning (string) const;
  void programming_error (string) const;


  /* class hierarchy */
  virtual System *get_system () const;
  virtual void do_break_processing ();
  virtual Grob *find_broken_piece (System *) const;
  virtual void discretionary_processing ();
  virtual void derived_mark () const;
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();

  /* printing */
  Stencil *get_stencil () const;
  Stencil get_print_stencil () const;

  /* interfaces */
  bool internal_has_interface (SCM intf);
  DECLARE_GROB_INTERFACE();

  /* offsets */
  void translate_axis (Real, Axis);
  Real relative_coordinate (Grob const *refp, Axis) const;
  Real pure_relative_y_coordinate (Grob const *refp, int start, int end);
  Real maybe_pure_coordinate (Grob const *refp, Axis a, bool pure, int start, int end);

  /* extents */
  Interval extent (Grob *refpoint, Axis) const;
  void flush_extent_cache (Axis);
  virtual Interval pure_height (Grob *refpoint, int start_col, int end_col);
  Interval maybe_pure_extent (Grob *refpoint, Axis, bool pure, int start, int end);

  /* refpoints */
  Grob *common_refpoint (Grob const *s, Axis a) const;
  void set_parent (Grob *e, Axis);
  Grob *get_parent (Axis a) const;
  void fixup_refpoint ();

  virtual Interval_t<int> spanned_rank_interval () const;
  bool check_cross_staff (Grob *common);
};

/* smob utilities */
DECLARE_UNSMOB (Grob, grob);
Spanner *unsmob_spanner (SCM);
Item *unsmob_item (SCM);

/* refpoints */
Grob *common_refpoint_of_list (SCM elt_list, Grob *, Axis a);
Grob *common_refpoint_of_array (vector<Grob*> const &, Grob *, Axis a);
System *get_root_system (Grob *me);

/* extents */ 
Interval robust_relative_extent (Grob *, Grob *, Axis);

/* offset/extent callbacks. */
void add_offset_callback (Grob *g, SCM proc, Axis a);
void chain_offset_callback (Grob *g, SCM proc, Axis a);
void chain_callback (Grob *g, SCM proc, SCM sym);
SCM axis_offset_symbol (Axis a);
SCM axis_parent_positioning (Axis a);

SCM call_pure_function (SCM unpure, SCM args, int start, int end);

void set_nested_property (Grob *, SCM property_path, SCM value);

#endif /* GROB_HH */
