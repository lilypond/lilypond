/*
  edge_out.hh -- declare Directed_graph_node

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef DEPENDENCY_HH
#define DEPENDENCY_HH

#include "parray.hh"

/**
  Class to handle two-sided connections between nodes (the Dependencies)
 */
class Directed_graph_node {
  Link_array<Directed_graph_node>edge_out_l_arr_;
  /// targets
  Link_array<Directed_graph_node> edge_in_l_arr_;
    
public:
    
  /** remove i-th edge_out (and exactly one ref to me in the edge_out)
   */
  void remove_edge_out_idx (int i);
  void copy_edges_out (Directed_graph_node const&);
  bool linked_b() const;
  void unlink();
  void junk_links();
  void add (Directed_graph_node*);
  void remove_edge_in (Directed_graph_node *);
  void remove_edge_out (Directed_graph_node*);
  bool contains_b (Directed_graph_node const*) const;

  Directed_graph_node (Directed_graph_node const &);
  void OK() const;
  Directed_graph_node();

  ~Directed_graph_node();
    
  /**
    ensure that no edge_out exists doubly.
    */
  void uniq();
  Link_array<Directed_graph_node> const& get_out_edge_arr() const;
  Link_array<Directed_graph_node> const& get_in_edge_arr() const;
};

#endif // DEPENDENCY_HH
