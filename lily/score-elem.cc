/*
  score-elem.cc -- implement Score_elem

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/
/*
  too big. Should split.
 */
#include "p-score.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "molecule.hh"
#include "score-elem.hh"
#include "debug.hh"
#include "tex.hh"
#include "dimen.hh"
#include "spanner.hh"
#include "scoreline.hh"
#include "item.hh"
#include "p-col.hh"

Score_elem::Score_elem()
{
  transparent_b_ = empty_b_ = false;
  axis_group_l_a_[0] = axis_group_l_a_[1] =0;
  pscore_l_=0;
  offset_ = Offset (0,0);
  output = 0;
  status_ = ORPHAN;
}

Score_elem::Score_elem (Score_elem const&s)
{
  /* called from derived ctor, so most info points to the same deps
     as (Directed_graph_node&)s. Nobody points to us, so don't copy
     dependents.      
   */
  copy_edges_out (s);
  transparent_b_ = s.transparent_b_;
  empty_b_ = s.empty_b_;
  axis_group_l_a_[0] = axis_group_l_a_[1] =0;
  status_ = s.status_;
  assert (!s.output);
  output = 0;
  pscore_l_ = s.pscore_l_;
  offset_ = Offset (0,0);
}

Score_elem::~Score_elem()
{
  // some paranoia to prevent weird segv's
  assert (status_ < DELETED);
  delete output;
  status_ = DELETED;
  output = 0;
}

Score_elem*
Score_elem::dependency (int i) const
{
  return (Score_elem*) get_out_edge_arr ()[i];
}

int
Score_elem::dependency_size () const
{
  return get_out_edge_arr ().size ();
}

Score_elem*
Score_elem::dependent (int i) const
{
  return (Score_elem*) get_in_edge_arr()[i];
}

int
Score_elem::dependent_size() const
{
  return get_in_edge_arr().size ();
}

String
Score_elem::make_TeX_string (Offset o)const
{
  String s ("\\placebox{%}{%}{%}");
  Array<String> a;
  a.push (print_dimen (o.y()));
  a.push (print_dimen (o.x()));
  String t = output->TeX_string();
  if (!t)
    return t;

  a.push (t);
  String r;
  if (check_debug)
    r = String ("\n%start: ") + name() + "\n";
  r += substitute_args (s, a);
  return r;
}
String
Score_elem::do_TeX_output_str () const
{
  return make_TeX_string(absolute_offset());
}

/*
  GEOMETRY
 */
Real
Score_elem::absolute_coordinate (Axis a) const
{
  Real r = offset_[a];
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
	axis_group_l; axis_group_l = axis_group_l->axis_group_l_a_[a])
	
    r += axis_group_l->offset_[a];
  return r;
}
 

Offset
Score_elem::absolute_offset() const
{
  return Offset (absolute_coordinate (X_AXIS), absolute_coordinate (Y_AXIS));
}

void
Score_elem::translate (Real y, Axis a)
{
  offset_[a] += y;
}

Real
Score_elem::relative_coordinate (Axis_group_element*e, Axis a) const
{
  Real r =0.0;
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
	axis_group_l != e;
	axis_group_l = axis_group_l->axis_group_l_a_[a])
    r +=  axis_group_l->offset_[a];

  return r;
}

Axis_group_element* 
Score_elem::common_group (Score_elem const* s, Axis a) const
{
  Link_array<Axis_group_element> my_groups;
  for (Axis_group_element * axis_group_l = axis_group_l_a_[a];
	axis_group_l;
	axis_group_l = axis_group_l->axis_group_l_a_[a])
    my_groups.push (axis_group_l);

  Axis_group_element* common_l=0;
  for (Axis_group_element * axis_group_l = s->axis_group_l_a_[a];
	!common_l && axis_group_l;
	axis_group_l = axis_group_l->axis_group_l_a_[a])
    common_l = my_groups.find_l (axis_group_l);

  return common_l;
}



void
Score_elem::translate (Offset O)
{
  offset_ += O;
}

Interval
Score_elem::do_width() const 
{
  Interval r;

  if (!output)
    {
      Molecule*m = brew_molecule_p();
      r = m->extent().x ();
      delete m;
    }
  else
    r = output->extent().x ();
  return r;
}

Interval
Score_elem::width() const
{
  return extent (X_AXIS);
}

Interval
Score_elem::extent (Axis a) const
{
  Interval r;
  if (!empty_b_) 
    {
	
      r = (a == X_AXIS)? do_width(): do_height ();
    }
  
  if (!r.empty_b()) // float exception on DEC Alpha
    r+=offset_[a];

  return r;
}

Interval
Score_elem::do_height() const 
{
  Interval r;
  if (!output)
    {
      Molecule*m = brew_molecule_p();
      r = m->extent().y ();
      delete m;
    }
  else
    r = output->extent().y ();
  return r;
}

Interval
Score_elem::height() const
{
  return extent (Y_AXIS);
}

/*
  STANDARD METHS
 */
void
Score_elem::print() const
{
#ifndef NPRINT
  DOUT << name() << "{\n";
  DOUT << "dets: " << dependent_size() << "dependencies: " << 
    dependency_size();
  if (offset_.x() || offset_.y ())
    DOUT << "offset (" << offset_.x() << ", " << offset_.y () <<")";
  DOUT << "\n";

  do_print();
  if (output)
    output->print();
  
  DOUT <<  "}\n";
#endif
}


Paper_def*
Score_elem::paper()  const
{
  assert (pscore_l_);
  return pscore_l_->paper_l_;
}

void
Score_elem::add_processing()
{
  if (status_ >= VIRGIN)
    return;
  status_ = VIRGIN;
  do_add_processing();
}

void
Score_elem::pre_processing()
{
  if (status_ >= PRECALCED)
    return;

  assert (status_ != PRECALCING); // cyclic dependency
  status_ = PRECALCING;

  for (int i=0; i < dependency_size(); i++)
    dependency (i)->pre_processing();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->pre_processing();
  
  do_pre_processing();
  status_ = PRECALCED;
}

void
Score_elem::breakable_col_processing()
{
  if (status_ >= PREBROKEN)
    return;

  if (status_== PREBREAKING) 
    {
      status_ = PREBROKEN;
      return ;
    }
  status_ = PREBREAKING;

  for (int i=0; i < dependency_size(); i++)
    dependency (i)->breakable_col_processing();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->breakable_col_processing();
  
  
  do_breakable_col_processing();
  status_ = PREBROKEN;
}

void
Score_elem::break_processing()
{
  if (status_ >= BROKEN)
    return;

  if (status_ == BREAKING) 
    {
      status_ = BROKEN;
      return;
    }
  status_ = BREAKING;

  for (int i=0; i < dependency_size(); i++)
    dependency (i)->break_processing();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->break_processing();
  

  
  do_break_processing();
  status_ = BROKEN;

}

void
Score_elem::do_break_processing()
{
  handle_broken_dependencies();
}


void
Score_elem::post_processing()
{
  if (status_ >= POSTCALCED)
    return;
  assert (status_ != POSTCALCING);// cyclic dependency
  status_=POSTCALCING;	

  
  for (int i=0; i < dependency_size(); i++)
    dependency (i)->post_processing();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->post_processing();
  

  do_post_processing();
  status_=POSTCALCED;
}

Score_elem::Status
Score_elem::status() const
{
  return status_;
}

void 
Score_elem::molecule_processing()
{
  if (status_ >= BREWED)
    return;
  status_ = BREWED;		// do it only once.
  
  for (int i=0; i < dependency_size(); i++)
    dependency (i)->molecule_processing();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->molecule_processing();
  

  if (transparent_b_)
    return ;
  output= brew_molecule_p();
}

String
Score_elem::TeX_output_str() const
{
  String s;
  if (status_ >= TEXOUTPUT)
    return "";

  ((Score_elem*)this)->status_ = TEXOUTPUT;

  for (int i=0; i < dependency_size(); i++)
    s += dependency (i)->TeX_output_str();

  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    s += extra[i]->TeX_output_str ();
  
  if (!transparent_b_)
    s+= do_TeX_output_str();

  return s;
}

/*
  
  VIRTUAL STUBS

 */
void
Score_elem::do_post_processing()
{
}

void
Score_elem::do_breakable_col_processing()
{
  handle_prebroken_dependencies();
}

void
Score_elem::do_pre_processing()
{
}

void
Score_elem::do_add_processing()
{
}

void
Score_elem::do_substitute_dependency (Score_elem*,Score_elem*)
{
}
void
Score_elem::do_substitute_dependent (Score_elem*,Score_elem*)
{
}

void
Score_elem::do_unlink()
{
}

void
Score_elem::do_junk_links()
{
}

IMPLEMENT_IS_TYPE_B(Score_elem);

Molecule*
Score_elem::brew_molecule_p() const
{
  Atom a (paper()->lookup_l ()->fill (Box (Interval (0,0), Interval (0,0))));
  return new Molecule (a);
}


Line_of_score *
Score_elem::line_l() const
{
  return 0;
}

/*
  
  DEPENDENCIES

  */

void
Score_elem::remove_dependency (Score_elem*e)
{
  remove_edge_out (e);
  substitute_dependency (e, 0);
}

void
Score_elem::add_dependency (Score_elem*e)
{
  Directed_graph_node::add (e);
}
void
Score_elem::substitute_dependency (Score_elem* old, Score_elem* new_l)
{
  do_substitute_dependency (old,new_l);
  old->do_substitute_dependent (this, 0);
}

void
Score_elem::handle_broken_dependencies()
{
  Line_of_score *line  = line_l();
  if (!line)
    return;

  Link_array<Score_elem> remove_us_arr;
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_elem * elt = dependency (i);
      if (elt->line_l() != line)
	{
	  if (elt->spanner()) 
	    {
	      Spanner * sp = elt->spanner();
	      Spanner * broken = sp->find_broken_piece (line);
	      substitute_dependency (sp, broken);

	      add_dependency (broken);
	    }
	  else if (elt->item())
	    {
	      Item * my_item = elt->item()->find_prebroken_piece (line);
		
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

  status_ = BROKEN;
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
Score_elem::handle_prebroken_dependencies()
{
  Link_array<Score_elem> old_arr, new_arr;
  
  for (int i=0; i < dependency_size(); i++) 
    {
      Score_elem * elt = dependency (i);
      Item *it_l = elt->item();
      if (it_l && it_l->breakable_b_)
	if (item()) 
	  {
	    Score_elem *new_l = it_l->find_prebroken_piece (item()->break_status_i_);
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
	
	
  status_ = PREBROKEN;
}



void
Score_elem::unlink_all()
{
  for (int i=0; i < dependency_size(); i++) 
    dependency (i)->unlink_all();
  Link_array<Score_elem> extra (get_extra_dependencies());
  for (int i=0; i < extra.size(); i++)
    extra[i]->unlink_all();
  
  junk_links();
  axis_group_l_a_[X_AXIS] = axis_group_l_a_[Y_AXIS] =0;
  do_unlink();
}

void
Score_elem::unlink()
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
  for (int j=0; j < 2; j++)
    if (axis_group_l_a_[j])
      axis_group_l_a_[j]->remove_element (this);

}

void
Score_elem::OK() const
{
#ifndef NDEBUG
  for (int i=0; i < dependency_size(); i++) 
    {
      dependency (i)->OK();
    }
#endif
}

Link_array<Score_elem>
Score_elem::get_extra_dependencies() const
{
  Link_array<Score_elem> empty;
  return empty;
}

bool
Score_elem::linked_b() const
{
  return get_extra_dependencies().size() || 
    dependency_size();
}
