/*
  score-element.hh -- declare Score_element

  (c) 1996-1999 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH

#include "parray.hh"
#include "virtual-methods.hh"
#include "directed-graph.hh"
#include "graphical-element.hh"
#include "protected-scm.hh"


typedef void (Score_element::*Score_element_method_pointer) (void);

/** Both Spanner and Item are Score_element's. Most Score_element's depend
  on other Score_element's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies fields of struct Score_element,
  which are implemented in the Directed_graph_node class: all elements
  form an acyclic graph.

  (elem) */
class Score_element : public virtual Graphical_element {
  Protected_scm element_property_alist_;
  Link_array<Score_element> dependency_arr_;
  
public:
  /// delete after linebreak calculation.
  bool break_helper_only_b_;
  Paper_score *pscore_l_;    
  Molecule * output_p_;
  Score_element ();
  Score_element (Score_element const&);
  virtual void print () const;

  SCM get_elt_property (SCM sym);
  void set_elt_property (SCM sym, SCM val);
  
  Paper_def *paper () const;
  Lookup const *lookup_l () const;

  virtual ~Score_element ();
  void add_processing ();

  void substitute_dependency (Score_element*,Score_element*);
  void remove_dependency (Score_element*);
  /**
    add a dependency. It may be the 0 pointer, in which case, it is ignored.
    */
  void add_dependency (Score_element*);    

  virtual Line_of_score * line_l () const;
  virtual bool linked_b () const;
  VIRTUAL_COPY_CONS(Score_element);
 
  /// do not print anything black
  bool transparent_b_;

  // ugh: no protection. Denk na, Vrij Veilig
  void calculate_dependencies (int final, int busy, Score_element_method_pointer funcptr);

public:
  /**
    Administration: Where are we?. This is mainly used by Super_element and
    Score_element::calcalute_dependencies ()

    0 means ORPHAN,
    -1 means deleted
    
   */
  int status_i_;

protected:
  Score_element* dependency (int) const;
  int dependency_size () const;
  
  virtual void output_processing ();
  virtual Interval do_height () const;
  virtual Interval do_width () const;
    
  /// do printing of derived info.
  virtual void do_print () const;
  /// generate the molecule    
  virtual Molecule* do_brew_molecule_p () const;
  ///executed directly after the item is added to the Paper_score
  virtual void do_add_processing ();
  /// do calculations before determining horizontal spacing
  virtual void do_pre_processing ();

  /// generate rods & springs
  virtual void do_space_processing ();

  virtual void do_breakable_col_processing ();
  /// do calculations after determining horizontal spacing
  virtual void do_post_processing ();
    
  virtual void do_substitute_element_pointer (Score_element * , Score_element *);
  virtual void do_break_processing ();
  virtual void handle_broken_dependencies ();
  virtual void handle_prebroken_dependencies ();
  virtual void handle_prebroken_dependents ();
  virtual Link_array<Score_element> get_extra_dependencies () const;
};


#endif // STAFFELEM_HH

