/*
  grob.hh -- declare Grob

  source file of the LilyPond music typesetter
  
  (c) 1996--2004 Han-Wen Nienhuys
*/

#ifndef GROB_HH
#define GROB_HH

#include "parray.hh"
#include "virtual-methods.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "dimension-cache.hh"
#include "grob-interface.hh"

/**
    for administration of what was done already
    */
enum Grob_status {
  ORPHAN=0,			// not yet added to Paper_score
  PRECALCING,
  PRECALCED,		// calcs before spacing done
  POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
  POSTCALCED,		// after spacing calcs done
};

typedef void (Grob::*Grob_method_pointer) (void);


/* Basic G[raphical output] O[bject].  */
class Grob
{
protected:
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  friend class Spanner;
  
  void substitute_mutable_properties(SCM,SCM);

public:
  Grob *original_;

  /**
    Administration: Where are we?. This is mainly used by Super_element and
    Grob::calcalute_dependencies ()

    0 means ORPHAN,
   */
  char status_;
  Paper_score *pscore_;
  Dimension_cache dim_cache_[NO_AXES];

  Grob (SCM basic_props);
  Grob (Grob const&);
  VIRTUAL_COPY_CONSTRUCTOR(Grob,Grob);
 
  String name () const;
  
  /*
    properties
   */
  SCM internal_get_property (SCM) const;
  void internal_set_property (SCM, SCM val);
  void add_to_list_property (SCM, SCM);
  void warning (String)const;
  void programming_error (String)const;
  
  /*
    related classes.
   */
  Output_def *get_paper () const;

  /**
    add a dependency. It may be the 0 pointer, in which case, it is ignored.
    */
  void add_dependency (Grob*);    
  virtual System * get_system () const;

  /**
     Recursively track all dependencies of this Grob.  The
     status_ field is used as a mark-field.  It is marked with
     #busy# during execution of this function, and marked with #final#
     when finished.

     #funcptr# is the function to call to update this element.
   */
  void calculate_dependencies (int final, int busy, SCM funcname);


  virtual void do_break_processing ();
  virtual Grob *find_broken_piece (System*) const;
  virtual void discretionary_processing ();
  virtual SCM do_derived_mark () const;

  Stencil * get_stencil () const;
  SCM get_uncached_stencil () const;

  SCM get_property_alist_chain (SCM) const;
  void suicide ();
  bool is_live () const;
  
  DECLARE_SCHEME_CALLBACK (stencil_extent, (SCM smob, SCM axis));

  static SCM ly_grob_set_property (SCM, SCM,SCM);
  static SCM ly_grob_property (SCM, SCM);  

  bool internal_has_interface (SCM intf);
  static bool has_interface (Grob*me);  

  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();

  DECLARE_SMOBS (Grob,foo);

  void init ();
public:
  bool is_empty (Axis a) const;

  Interval extent (Grob * refpoint, Axis) const;
 
  void translate_axis (Real, Axis);
  Real relative_coordinate (Grob const* refp, Axis) const;
  Grob*common_refpoint (Grob const* s, Axis a) const;

  // duh. slim down interface here. (todo)
  bool has_offset_callback (SCM callback, Axis)const;
  void add_offset_callback (SCM callback, Axis);
  bool has_extent_callback (SCM, Axis)const;  
  void set_extent (SCM , Axis);
  Real get_offset (Axis a) const;
  
  void set_parent (Grob* e, Axis);
  Grob *get_parent (Axis a) const {   return  dim_cache_[a].parent_; }

  DECLARE_SCHEME_CALLBACK (fixup_refpoint, (SCM));
};

DECLARE_UNSMOB(Grob,grob);
Spanner* unsmob_spanner (SCM );
Item* unsmob_item (SCM );

Grob*common_refpoint_of_list (SCM elt_list, Grob * , Axis a);
Grob*common_refpoint_of_array (Link_array<Grob> const&, Grob * , Axis a);

void set_break_subsititution (SCM criterion);
SCM substitute_mutable_property_alist (SCM alist);

Link_array<Grob> ly_scm2grobs (SCM l);
SCM ly_grobs2scm (Link_array<Grob> a);

#endif /* GROB_HH */

