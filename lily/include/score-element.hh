/*
  score-element.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH

#include "parray.hh"
#include "virtual-methods.hh"
#include "directed-graph.hh"
#include "graphical-element.hh"

#define SCORE_ELEMENT_CLONE(T) VIRTUAL_COPY_CONS (T, Score_element)


typedef void (Score_element::*Score_element_method_pointer) (void);

/** Both Spanner and Item are Score_element's. Most Score_element's depend
  on other Score_element's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies fields of struct Score_element,
  which are implemented in the Directed_graph_node class: all elements
  form an acyclic graph.

  (elem) */
class Score_element : private Directed_graph_node, public virtual Graphical_element {
public:
  Paper_score *pscore_l_;    

  Score_element ();
  Score_element (Score_element const&);
  virtual void print () const;
    
  Paper_def *paper () const;
  Lookup const *lookup_l () const;

  virtual ~Score_element ();
  DECLARE_MY_RUNTIME_TYPEINFO;    

  void add_processing ();

  /**
    Remove all  links (dependencies, dependents, Axis_group_elements.
    */
  void unlink ();
  void substitute_dependency (Score_element*,Score_element*);
  void remove_dependency (Score_element*);
  /**
    add a dependency. It may be the 0 pointer, in which case, it is ignored.
    */
  void add_dependency (Score_element*);    

  /*
    virtual accessors
    */

  virtual Spanner* access_Spanner ()  { return 0; }
  virtual Span_bar* access_Span_bar ()  { return 0; }
  virtual Axis_group_element * access_Axis_group_element () { return 0; }
  virtual Score_element* access_Score_element () { return this; }
  
  virtual Item * access_Item () { return 0; }
  virtual Line_of_score * line_l () const;
  virtual bool linked_b () const;
  SCORE_ELEMENT_CLONE (Score_element);
 
  /// do not print anything black
  bool transparent_b_;

  int size_i_;
  
  // ugh: no protection. Denk na, Vrij Veilig
  void calculate_dependencies (int final, int busy, Score_element_method_pointer funcptr);

protected:
  /**
    Administration: Where are we?. This is mainly used by Super_element and
    Score_element::calcalute_dependencies ()

    0 means ORPHAN,
    -1 means deleted
    
   */
public:
  int status_i_;
protected:
  Score_element* dependency (int) const;
  Score_element* dependent (int) const;
  int dependent_size () const;
  int dependency_size () const;
  
  virtual void do_brew_molecule ();
  void junk_links ();
  virtual Interval do_height () const;
  virtual Interval do_width () const;
    
  /// do printing of derived info.
  virtual void do_print () const {}
  /// generate the molecule    
  virtual Molecule* brew_molecule_p () const;
  ///executed directly after the item is added to the Paper_score
  virtual void do_add_processing ();
  /// do calculations before determining horizontal spacing
  virtual void do_pre_processing ();

  /// generate rods & springs
  virtual void do_space_processing ();

  virtual void do_breakable_col_processing ();
  /// do calculations after determining horizontal spacing
  virtual void do_post_processing ();
    
  virtual void do_substitute_dependency (Score_element * , Score_element *);
  virtual void do_substitute_dependent (Score_element *, Score_element *);
  virtual void do_break_processing ();
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();
  virtual Link_array<Score_element> get_extra_dependencies () const;
  virtual void do_unlink ();
  virtual void do_junk_links ();
  
};


#endif // STAFFELEM_HH

