/*
  score-element.hh -- declare Score_element

  (c) 1996-1999--2000 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH

#include "parray.hh"
#include "virtual-methods.hh"
#include "lily-guile.hh"
#include "lily-proto.hh"
#include "smobs.hh"
#include "dimension-cache.hh"

typedef Interval (*Extent_callback)(Score_element *,Axis);
typedef Real (*Offset_callback)(Score_element *,Axis);



/**
    for administration of what was done already
    */
enum Score_element_status {
  ORPHAN=0,			// not yet added to pstaff
  VIRGIN,			// added to pstaff
  PRECALCING,
  PRECALCED,		// calcs before spacing done
  POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
  POSTCALCED,		// after spacing calcs done
  BREWING,
  BREWED,
};

typedef void (Score_element::*Score_element_method_pointer) (void);

/**
   Basic output object.

    Element Properties:

   transparent -- boolean: if true, do not print anything black.

   dependencies -- list of score-element pointers that indicate who to
   compute first.

   interfaces -- list of symbols indicating the interfaces supported
   by this object.

   extra-offset -- pair of reals (a cons) forcing an extra offset
   before outputting

   glyph -- afm character name to output.
   
*/
class Score_element  {
  /**
     The lookup, determined by the font size. Cache this value.
   */
  Lookup * lookup_l_;

public:
  SCM immutable_property_alist_;
  SCM mutable_property_alist_;
  Score_element *original_l_;

  /**
    Administration: Where are we?. This is mainly used by Super_element and
    Score_element::calcalute_dependencies ()

    0 means ORPHAN,
   */
  char status_i_;
  char const * name () const;

  /*
    IDEA: make this a global variable. This is the same for all
    elements, I think it is safe to assume that we will not have
    scores being formatted multithreadedly.
   */
  Paper_score *pscore_l_;

  Score_element (SCM basic_props);
  Score_element (Score_element const&);

  /*
    properties
   */
  SCM get_elt_property (const char*) const;
  SCM get_elt_property (SCM) const;
  void set_elt_property (const char * , SCM val);
  void set_immutable_elt_property (const char * , SCM val);
  void set_immutable_elt_property (SCM key, SCM val);  
  void set_elt_property (SCM , SCM val);  
  void set_elt_pointer (const char*, SCM val);
  friend class Property_engraver; //  UGHUGHUGH.
  SCM remove_elt_property (const char* nm);

  /*
    related classes.
   */
  Paper_def *paper_l () const;
  Lookup const *lookup_l () const;

  void add_processing ();

  /**
    add a dependency. It may be the 0 pointer, in which case, it is ignored.
    */
  void add_dependency (Score_element*);    
  virtual Line_of_score * line_l () const;
  bool linked_b () const;


  VIRTUAL_COPY_CONS(Score_element);
 
  /**
     Recursively track all dependencies of this Score_element.  The
     status_i_ field is used as a mark-field.  It is marked with
     #busy# during execution of this function, and marked with #final#
     when finished.

     #funcptr# is the function to call to update this element.
   */
  void calculate_dependencies (int final, int busy, SCM funcname);


  static SCM handle_broken_smobs (SCM, SCM criterion);

  virtual void do_break_processing ();
  virtual Score_element *find_broken_piece (Line_of_score*) const;
  virtual void discretionary_processing ();
  virtual void do_derived_mark ();

  Molecule get_molecule () const;
  void suicide ();
  
  static Interval preset_extent (Score_element *,Axis);
  static Interval point_dimension_callback (Score_element *,Axis );
  static Interval molecule_extent (Score_element *,Axis);

protected:
  /**
    Junk score element. This is protected because this is supposed to
    be handled by GUILE gc.  */
  virtual ~Score_element ();
  
  ///executed directly after the item is added to the Paper_score
  virtual void do_add_processing ();
  static Interval dim_cache_callback (Dimension_cache const*);
  
public:
  static SCM ly_set_elt_property (SCM, SCM,SCM);
  static SCM ly_get_elt_property (SCM, SCM);  

  bool has_interface (SCM intf);
  void set_interface (SCM intf);


  static SCM brew_molecule (SCM);
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();


  DECLARE_SMOBS;

  void init ();

  Dimension_cache dim_cache_[NO_AXES];

public:
  bool empty_b (Axis a) const;
  Interval extent (Axis) const;
 
  /**
    translate in one direction
    */
    
  void translate_axis (Real, Axis);

  /**
     Find the offset relative to D.  If   D equals THIS, then it is 0.
     Otherwise, it recursively defd as

     OFFSET_ + PARENT_L_->relative_coordinate (D)
   */
  Real relative_coordinate (Score_element const* refp, Axis) const;
  /**
    Find the group-element which has both #this# and #s#
   */
  Score_element*common_refpoint (Score_element const* s, Axis a) const;
  Score_element*common_refpoint (SCM elt_list, Axis a) const;

  bool has_offset_callback_b (Offset_callback, Axis)const;
  void add_offset_callback (Offset_callback, Axis);
  bool has_extent_callback_b (Extent_callback, Axis)const;  
  void set_extent_callback (Extent_callback , Axis);

  /**
    Invoke callbacks to get offset relative to parent.
   */
  Real get_offset (Axis a) const;
  /**
     Set the  parent refpoint of THIS to E
   */
  void set_parent (Score_element* e, Axis);
  
  Score_element *parent_l (Axis a) const;
  static SCM fixup_refpoint (SCM);
};

Score_element * unsmob_element (SCM);

#endif // STAFFELEM_HH

