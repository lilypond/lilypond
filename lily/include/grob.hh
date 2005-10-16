/*
  grob.hh -- declare Grob

  source file of the LilyPond music typesetter

  (c) 1996--2005 Han-Wen Nienhuys
*/

#ifndef GROB_HH
#define GROB_HH

#include "virtual-methods.hh"
#include "dimension-cache.hh"
#include "grob-interface.hh"
#include "object-key.hh"

typedef void (Grob:: *Grob_method_pointer) (void);

class Grob
{
private:
  DECLARE_SMOBS (Grob, foo);
  void init ();

protected:
  Object_key const *key_;
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  SCM object_alist_;
  SCM property_callbacks_;
  
  /*
    If this is a property, it accounts for 25% of the property
    lookups.
  */
  SCM interfaces_;

  /* BARF */
  friend class Spanner;
  friend SCM ly_grob_properties (SCM);
  friend SCM ly_grob_basic_properties (SCM);
  friend void check_interfaces_for_property (Grob const *, SCM);
  void substitute_object_links (SCM, SCM);
  char status_;

  DECLARE_CLASSNAME(Grob);
public:
  DECLARE_SCHEME_CALLBACK(same_axis_parent_positioning, (SCM, SCM));
  DECLARE_SCHEME_CALLBACK(other_axis_parent_positioning, (SCM, SCM));

  Object_key const *get_key () const;

  Grob *original_;

  /* TODO: junk this member. */
  Paper_score *pscore_;

  Dimension_cache dim_cache_[NO_AXES];

  Grob (SCM basic_props, Object_key const *);
  Grob (Grob const &, int copy_count);

  virtual Grob *clone (int count) const;
  DECLARE_SCHEME_CALLBACK (stencil_extent, (SCM smob, SCM axis));
  
  String name () const;
  /*
    Properties
  */
  SCM internal_get_property (SCM symbol) const;
  SCM get_property_data (SCM symbol) const;
  SCM internal_get_object (SCM symbol) const;

  void internal_set_property (SCM sym, SCM val);
  void internal_set_object (SCM sym, SCM val);

  SCM try_callback (SCM);
  /*
    JUNKME.
  */
  void add_to_list_property (SCM, SCM);
  void add_to_object_list (SCM sym, SCM thing);

  SCM get_property_alist_chain (SCM) const;
  static SCM ly_grob_set_property (SCM, SCM, SCM);
  static SCM ly_grob_property (SCM, SCM);

  void warning (String) const;
  void programming_error (String) const;

  Output_def *get_layout () const;
  void add_dependency (Grob *);
  virtual System *get_system () const;

  void calculate_dependencies (int final, int busy, SCM funcname);

  virtual void do_break_processing ();
  virtual Grob *find_broken_piece (System *) const;
  virtual void discretionary_processing ();
  virtual void derived_mark () const;

  Stencil *get_stencil () const;
  Stencil get_print_stencil () const;

  void suicide ();
  bool is_live () const;
  bool is_empty (Axis a) const;

  bool internal_has_interface (SCM intf);
  static bool has_interface (Grob *me);
  SCM get_interfaces () const;
  
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();

  Interval extent (Grob *refpoint, Axis) const;

  void translate_axis (Real, Axis);
  Real relative_coordinate (Grob const *refp, Axis) const;
  Grob *common_refpoint (Grob const *s, Axis a) const;

  // duh. slim down interface here. (todo)
  void set_callback (SCM sym, SCM proc);
  bool has_offset_callback (SCM callback, Axis) const;
  void add_offset_callback (SCM callback, Axis);
  bool has_extent_callback (SCM, Axis) const;
  void flush_extent_cache (Axis);
  void set_extent (SCM, Axis);
  void set_extent_callback (SCM, Axis);
  Real get_offset (Axis a) const;

  void set_parent (Grob *e, Axis);

  // URG
  Grob *get_parent (Axis a) const;
  void fixup_refpoint ();
};

DECLARE_UNSMOB (Grob, grob);
Spanner *unsmob_spanner (SCM);
Item *unsmob_item (SCM);

Grob *common_refpoint_of_list (SCM elt_list, Grob *, Axis a);
Grob *common_refpoint_of_array (Link_array<Grob> const &, Grob *, Axis a);

void set_break_subsititution (SCM criterion);
SCM substitute_object_alist (SCM alist, SCM dest);

Link_array<Grob> ly_scm2grobs (SCM ell);
SCM ly_grobs2scm (Link_array<Grob> a);

Interval robust_relative_extent (Grob *, Grob *, Axis);

#endif /* GROB_HH */
