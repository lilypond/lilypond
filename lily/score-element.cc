/*
  score-elem.cc -- implement Score_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <string.h>

#include "tex-outputter.hh"
#include "p-score.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "score-element.hh"
#include "debug.hh"
#include "spanner.hh"
#include "line-of-score.hh"
#include "item.hh"
#include "p-col.hh"
#include "molecule.hh"
#include "misc.hh"

#define PARANOID

Score_element::Score_element()
{
  transparent_b_ = false;
  size_i_ = 0;
  pscore_l_=0;
  status_i_ = 0;
}

Score_element::Score_element (Score_element const&s)
  :  Directed_graph_node (s), Graphical_element (s)
{
  /* called from derived ctor, so most info points to the same deps
     as (Directed_graph_node&)s. Nobody points to us, so don't copy
     dependents.      
   */
  copy_edges_out (s);
  transparent_b_ = s.transparent_b_;
  status_i_ = s.status_i_;
  pscore_l_ = s.pscore_l_;
  size_i_ = s.size_i_;
}


  

Score_element::~Score_element()
{
  assert (status_i_ >=0);
}

Score_element*
Score_element::dependency (int i) const
{
  return (Score_element*) get_out_edge_arr ()[i];
}

int
Score_element::dependency_size () const
{
  return get_out_edge_arr ().size ();
}

Score_element*
Score_element::dependent (int i) const
{
  return (Score_element*) get_in_edge_arr()[i];
}

int
Score_element::dependent_size() const
{
  return get_in_edge_arr().size ();
}



Interval
Score_element::do_width() const 
{
  Interval r;

  Molecule*m = brew_molecule_p();
  r = m->extent().x ();
  delete m;
  
  return r;
}

Interval
Score_element::do_height() const 
{
  Interval r;
  Molecule*m = brew_molecule_p();
  r = m->extent().y ();
  delete m;
  return r;
}


/*
  STANDARD METHS
 */
void
Score_element::print() const
{
#ifndef NPRINT
  DOUT << name() << "{\n";
  DOUT << "dets: " << dependent_size() << "dependencies: " << 
    dependency_size();
 
  Graphical_element::do_print ();
  do_print();
  
  DOUT <<  "}\n";
#endif
}


Paper_def*
Score_element::paper()  const
{
  assert (pscore_l_);
  return pscore_l_->paper_l_;
}


Lookup const *
Score_element::lookup_l () const
{
  return pscore_l_->paper_l_->lookup_l (size_i_);
}

void
Score_element::add_processing()
{
  if (status_i_)
    return;
  status_i_ ++;
  do_add_processing();
}


void
Score_element::calculate_dependencies (int final, int busy,
				    Score_element_method_pointer funcptr)
{
  if (status_i_ >= final)
    return;

  assert (status_i_!= busy);
  status_i_= busy;

  for (int i=0; i < dependency_size(); i++)
    dependency (i)->calculate_dependencies (final, busy, funcptr);

  Link_array<Score_element> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->calculate_dependencies (final, busy, funcptr);
  
  invalidate_cache (X_AXIS);
  invalidate_cache (Y_AXIS);
  (this->*funcptr)();
  status_i_= final;
}

void
Score_element::do_brew_molecule () 
{
  if (transparent_b_)
    return;
  Molecule *output= brew_molecule_p ();
  pscore_l_->outputter_l_->output_molecule (output, absolute_offset (), name());
  delete output;
}

/*
  
  VIRTUAL STUBS

 */

void
Score_element::do_break_processing()
{
  handle_broken_dependencies();
}

void
Score_element::do_post_processing()
{
}

void
Score_element::do_breakable_col_processing()
{
  handle_prebroken_dependencies();
}

void
Score_element::do_pre_processing()
{
}

void
Score_element::do_space_processing ()
{
}

void
Score_element::do_add_processing()
{
}

void
Score_element::do_substitute_dependency (Score_element*,Score_element*)
{
}
void
Score_element::do_substitute_dependent (Score_element*,Score_element*)
{
}

void
Score_element::do_unlink()
{
}

void
Score_element::do_junk_links()
{
}

IMPLEMENT_IS_TYPE_B1(Score_element, Graphical_element);

Molecule*
Score_element::brew_molecule_p() const
{
  Atom a (lookup_l ()->fill (Box (Interval (0,0), Interval (0,0))));
  return new Molecule (a);
}


Line_of_score *
Score_element::line_l() const
{
  return 0;
}

/*
  
  DEPENDENCIES

  */

void
Score_element::remove_dependency (Score_element*e)
{
  remove_edge_out (e);
  substitute_dependency (e, 0);
}

void
Score_element::add_dependency (Score_element*e)
{
  Directed_graph_node::add_edge (e);
}
void
Score_element::substitute_dependency (Score_element* old, Score_element* new_l)
{
  do_substitute_dependency (old,new_l);
  old->do_substitute_dependent (this, 0);
}

void
Score_element::handle_broken_dependencies()
{
  Line_of_score *line  = line_l();
  if (!line)
    return;

  Link_array<Score_element> remove_us_arr;
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_element * elt = dependency (i);
      if (elt->line_l() != line)
	{
	  if (elt->access_Spanner ()) 
	    {
	      Spanner * sp = elt->access_Spanner ();
	      Spanner * broken = sp->find_broken_piece (line);
	      substitute_dependency (sp, broken);

	      add_dependency (broken);
	    }
	  else if (elt->access_Item ())
	    {
	      Item * my_item = elt->access_Item ()->find_prebroken_piece (line);
		
	      substitute_dependency (elt, my_item);
	      if (my_item)
		add_dependency (my_item);
	    }
	  remove_us_arr.push (elt);
	}
    }

  remove_us_arr.default_sort();
  remove_us_arr.uniq();
  for (int i=0;  i <remove_us_arr.size(); i++)
    remove_dependency (remove_us_arr[i]);
}

/*
  This sux.

  unlike with spanners, the number of items can increase

  span: item1

  becomes

  span: item1 item2 item3

  How to let span (a derived class) know that this happened?
 */
void
Score_element::handle_prebroken_dependencies()
{
  Link_array<Score_element> old_arr, new_arr;
  
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_element * elt = dependency (i);
      Item *it_l = elt->access_Item ();
      if (it_l && it_l->breakable_b_)
	if (access_Item ()) 
	  {
	    Score_element *new_l = it_l->find_prebroken_piece (access_Item ()->break_status_dir_);
	    if (new_l != elt) 
	      {
		new_arr.push (new_l);
		old_arr.push (elt);
	      }
	  }
	else 
	  {
	    new_arr.push (it_l->broken_to_drul_[LEFT]);
	    old_arr.push (0);
	    old_arr.push (0);		
	    new_arr.push (it_l->broken_to_drul_[RIGHT]);		
	  }
    }
  
  for (int i=0;  i < old_arr.size(); i++)
    if (old_arr[i])
      substitute_dependency (old_arr[i], new_arr[i]);
}


void
Score_element::junk_links ()
{
  Directed_graph_node::junk_links();
  Graphical_element::junk_links ();
  do_junk_links();
}

void
Score_element::unlink()
{
  do_unlink();
  while (dependency_size()) 
    {
      do_substitute_dependency (dependency (0),0);
      remove_edge_out_idx (0);
    }
  while  (dependent_size()) 
    {
      dependent (0)->remove_dependency (this);
    }
  Graphical_element::unlink ();
}


Link_array<Score_element>
Score_element::get_extra_dependencies() const
{
  Link_array<Score_element> empty;
  return empty;
}

bool
Score_element::linked_b() const
{
  return get_extra_dependencies().size() || 
    dependency_size();
}
