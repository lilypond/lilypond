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
#include "dimension-cache-callback.hh"





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
  /**
     properties specific for this element. Destructor will not call
     scm_unprotect, so as to allow more flexible GC arrangements.  The
     real alist is in (cdr element_property_alist_), to reduce the
     need for more scm_protect calls.

  */
  SCM element_property_alist_;
  
  Score_element *original_l_;

  /**
    Administration: Where are we?. This is mainly used by Super_element and
    Score_element::calcalute_dependencies ()

    0 means ORPHAN,
    -1 means deleted
    
   */
  int status_i_;

  Paper_score *pscore_l_;

  Score_element ();
  Score_element (Score_element const&);
  virtual void print () const;

  /*
    properties
   */
  SCM get_elt_property (String nm) const;
  void set_elt_property (String, SCM val);

  /**
     UGH! JUNKME ?

     This gets messy because it changes state

     calling 

     Bar::proc ()
     {
       s->remove_elt_property ("foo")
     } 

     twice may do weird things if Bar::foo has a default set.
     
   */
  SCM remove_elt_property (String nm);

  void Score_element::set_real (String, Real);
  Real Score_element::get_real (String s) const;

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
  void calculate_dependencies (int final, int busy, Score_element_method_pointer funcptr);


  static SCM handle_broken_smobs (SCM, SCM criterion);
  void recurse_into_smobs (SCM s, void (Score_element::*meth_ptr)());

  virtual void do_break_processing ();
  virtual Score_element *find_broken_piece (Line_of_score*) const;
  /// generate rods & springs
  virtual void do_space_processing ();
  virtual void do_breakable_col_processing ();

  /// do calculations before determining horizontal spacing
  virtual void before_line_breaking ();
  /// do calculations after determining horizontal spacing
  virtual void after_line_breaking ();
  virtual void output_processing ();
  

  static Interval preset_extent (Dimension_cache const*);
  static Interval molecule_extent (Dimension_cache const*);

protected:

  /**
    Junk score element. This is protected because this is supposed to
    be handled by GUILE gc.  */
  virtual ~Score_element ();
  
  /// do printing of derived info.
  virtual void do_print () const;
  /// generate the molecule    
  virtual Molecule do_brew_molecule () const;
  ///executed directly after the item is added to the Paper_score
  virtual void do_add_processing ();

    
  static Interval dim_cache_callback (Dimension_cache const*);
  
public:
  static SCM ly_set_elt_property (SCM, SCM,SCM);
  static SCM ly_get_elt_property (SCM, SCM);  

  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();


  DECLARE_SMOBS;

  void init ();

public:
  Dimension_cache *dim_cache_[NO_AXES];

  /**
     Set this if anyone points to me, or if I point to anyone.

     JUNKME.
   */
  bool used_b_;
  
  char const * name () const;

  bool empty_b (Axis a) const;
  Interval extent (Axis) const;
 
  /**
    translate in one direction
    */
    
  void translate_axis (Real, Axis);

  Real relative_coordinate (Score_element const* refp, Axis) const;
  /**
    Find the group-element which has both #this# and #s#
   */
  Score_element*common_refpoint (Score_element const* s, Axis a) const;
  Score_element*common_refpoint (Link_array<Score_element> elems, Axis a) const;


  bool has_offset_callback_b (Offset_cache_callback, Axis)const;
  void add_offset_callback (Offset_cache_callback, Axis);
  void set_extent_callback (Dim_cache_callback , Axis);
  
  /**
     Set the  parent refpoint of THIS to E
   */
  void set_parent (Score_element* e, Axis);
  
  Score_element *parent_l (Axis a) const;
  void fixup_refpoint ();
};

Score_element * unsmob_element (SCM);



#endif // STAFFELEM_HH

