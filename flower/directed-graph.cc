/*
  edge_out.cc -- implement Directed_graph_node

  source file FlowerLib

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "directed-graph.hh"

#ifdef PARANOID			// these checks eat huge amounts of time.
#define PARANOID_OK() OK()
#else
#define PARANOID_OK()
#endif


Link_array<Directed_graph_node> const &
Directed_graph_node::get_in_edge_arr() const
{
  return edge_in_l_arr_;
}

Link_array<Directed_graph_node> const &
Directed_graph_node::get_out_edge_arr() const
{
  return edge_out_l_arr_;
}

/**
  Should not copy deps automatically
 */
Directed_graph_node::Directed_graph_node (Directed_graph_node const&)
{
}

void
Directed_graph_node::copy_edges_out (Directed_graph_node const &s)
{
  for (int i=0; i < s.edge_out_l_arr_.size(); i++)
    add (s.edge_out_l_arr_[i]);
}

void
Directed_graph_node::OK() const
{ 
#ifndef NDEBUG
  for (int i=0; i < edge_out_l_arr_.size(); i++) 
    {
      assert (edge_out_l_arr_[i]->
	      edge_in_l_arr_.find_l (this));
    }
  for (int i=0; i < edge_in_l_arr_.size(); i++)
    assert (edge_in_l_arr_[i]->contains_b (this));
#endif
}

bool
Directed_graph_node::contains_b (const Directed_graph_node *d) const
{
  return edge_out_l_arr_.find_l ((Directed_graph_node*)d);
}
  
void
Directed_graph_node::remove_edge_out_idx (int i)
{
  PARANOID_OK();
  Directed_graph_node * d_l = edge_out_l_arr_.get (i);

  int j = d_l->edge_in_l_arr_.find_i (this);
  assert (j>=0);
  d_l->edge_in_l_arr_.unordered_del (j);
  PARANOID_OK();
}

void
Directed_graph_node::remove_edge_in (Directed_graph_node *d_l)
{
  PARANOID_OK();
  d_l->remove_edge_out (this);
  PARANOID_OK();
}
 
void
Directed_graph_node::remove_edge_out (Directed_graph_node *d_l)
{
  PARANOID_OK();
  for (int i=0; i < edge_out_l_arr_.size();) 
    {
      if (edge_out_l_arr_[i]== d_l)
	remove_edge_out_idx (i);
      else
	i++;
    }
  PARANOID_OK();
}
bool
Directed_graph_node::linked_b() const
{
  return edge_out_l_arr_.size() || edge_in_l_arr_.size ();
}

void
Directed_graph_node::junk_links()
{
  edge_in_l_arr_.set_size (0);
  edge_out_l_arr_.set_size (0);
}


void
Directed_graph_node::unlink()
{
#ifdef PARANOID
  PARANOID_OK();

  Link_array<Directed_graph_node> t = edge_out_l_arr_;
  t.concat (edge_in_l_arr_);
#endif

  while (edge_out_l_arr_.size())
    remove_edge_out_idx (0);
	
  while (edge_in_l_arr_.size())
    remove_edge_in (edge_in_l_arr_[0]);

#ifdef PARANOID
  for (int i =0; i < t.size(); i++)
    t[i]->OK();
#endif
}

Directed_graph_node::~Directed_graph_node()
{
  assert (!linked_b());
}

  
void
Directed_graph_node::add (Directed_graph_node* dep_l)
{
  PARANOID_OK();
  if (!dep_l)
    return ;
  dep_l->edge_in_l_arr_.push (this);
  edge_out_l_arr_.push (dep_l);
  PARANOID_OK();
}
  

Directed_graph_node::Directed_graph_node()
{
}

