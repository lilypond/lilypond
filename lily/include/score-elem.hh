/*
  score-elem.hh -- part of GNU LilyPond

  (c) 1996,97 Han-Wen Nienhuys
*/

#ifndef STAFFELEM_HH
#define STAFFELEM_HH

#include "parray.hh"
#include "virtual-methods.hh"
#include "directed-graph.hh"
#include "graphical-element.hh"

#define SCORE_ELEM_CLONE(T) VIRTUAL_COPY_CONS(T, Score_elem)


typedef void (Score_elem::*Score_elem_method_pointer)(void);

/** Both Spanner and Item are Score_elem's. Most Score_elem's depend
  on other Score_elem's, eg, Beam needs to know and set direction of
  Stem. So the Beam has to be calculated *before* Stem. This is
  accomplished with the dependencies fields of struct Score_elem,
  which are implemented in the Directed_graph_node class: all elements
  form an acyclic graph.

  (elem) */
class Score_elem : private Directed_graph_node, public Graphical_element {
public:
  Paper_score *pscore_l_;    

  Score_elem();
  Score_elem (Score_elem const&);
  virtual void print() const;
    
  Paper_def *paper() const;

  virtual ~Score_elem();
  DECLARE_MY_RUNTIME_TYPEINFO;    

  void add_processing();

  /**
    Remove all  links (dependencies, dependents, Axis_group_elements.
    */
  void unlink();
  void substitute_dependency (Score_elem*,Score_elem*);
  void remove_dependency (Score_elem*);
  /**
    add a dependency. It may be the 0 pointer, in which case, it is ignored.
    */
  void add_dependency (Score_elem*);    

  /*
    virtual accessors
    */

  virtual Spanner* spanner()  { return 0; }
  virtual Item * item() { return 0; }
  virtual Line_of_score * line_l() const;
  virtual bool linked_b() const;
  SCORE_ELEM_CLONE(Score_elem);
 
  /// do not print anything black
  bool transparent_b_;
  
  // ugh: no protection. Denk na, Vrij Veilig
  void calcalute_dependencies (int final, int busy, Score_elem_method_pointer funcptr);

protected:
  /**
    Administration: Where are we?. This is mainly used by Super_elem and
    Score_elem::calcalute_dependencies ()

    0 means ORPHAN,
    -1 means deleted
    
   */
  int status_i_;

  Score_elem* dependency (int) const;
  Score_elem* dependent (int) const;
  int dependent_size() const;
  int dependency_size() const;
  
  virtual void do_brew_molecule ();
  void junk_links();
  virtual Interval do_height() const;
  virtual Interval do_width() const;
    
  /// do printing of derived info.
  virtual void do_print() const {}
  /// generate the molecule    
  virtual Molecule* brew_molecule_p() const;
  ///executed directly after the item is added to the Paper_score
  virtual void do_add_processing();
  /// do calculations before determining horizontal spacing
  virtual void do_pre_processing();

  /// generate rods & springs
  virtual void do_space_processing ();

  virtual void do_breakable_col_processing();
  /// do calculations after determining horizontal spacing
  virtual void do_post_processing();
    
  virtual void do_substitute_dependency (Score_elem * , Score_elem *);
  virtual void do_substitute_dependent (Score_elem *, Score_elem *);
  virtual void do_break_processing();
  virtual void handle_broken_dependencies();
  virtual void handle_prebroken_dependencies();
  virtual Link_array<Score_elem> get_extra_dependencies() const;
  virtual void do_unlink();
  virtual void do_junk_links();
};


#endif // STAFFELEM_HH

