/*
  graphical-element.cc -- implement Graphical_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "dimension-cache.hh"
#include "graphical-element.hh"
#include "graphical-axis-group.hh"
#include "debug.hh"

Graphical_element::Graphical_element ()
{
  dim_cache_[X_AXIS] = new Dimension_cache;
  dim_cache_[Y_AXIS] = new Dimension_cache;
  used_b_ = false;
  init ();
}

Graphical_element::Graphical_element (Graphical_element const &s)
{
  dim_cache_[X_AXIS] = new Dimension_cache (*s.dim_cache_[X_AXIS]);
  dim_cache_[Y_AXIS] = new Dimension_cache (*s.dim_cache_[Y_AXIS]);

  used_b_ = true;
  init ();
} 

void
Graphical_element::init ()
{
  dim_cache_[X_AXIS]->elt_l_ = dim_cache_[Y_AXIS]->elt_l_ = this;  
}


void
Graphical_element::translate_axis (Real y, Axis a)
{
  dim_cache_[a]->translate (y);
}  

Real
Graphical_element::relative_coordinate (Graphical_element const*e, Axis a) const
{
  return dim_cache_[a]->relative_coordinate (e ? e->dim_cache_[a] : 0);
}

Graphical_element * 
Graphical_element::common_refpoint (Graphical_element const* s, Axis a) const
{
  return  (dim_cache_[a]->common_refpoint (s->dim_cache_[a])) ->element_l ();
}

void
Graphical_element::translate (Offset offset)
{
  translate_axis (offset[Y_AXIS], Y_AXIS);
  translate_axis (offset[X_AXIS], X_AXIS);
}


void
Graphical_element::set_empty (bool b, Axis a1, Axis a2)
{
  if (a1 != NO_AXES)
    dim_cache_[a1]->set_empty (b);
  if (a2 != NO_AXES)
    dim_cache_[a2]->set_empty (b);
}

/**
   Return true if empty in either direction.
 */
bool
Graphical_element::empty_b (Axis a1, Axis a2)
{
  bool b = false;
  if (a1 != NO_AXES)
    b = b || dim_cache_[a1]->empty_b ();
  if (a2 != NO_AXES)
    b = b || dim_cache_[a2]->empty_b ();
  return b;
}

Interval
Graphical_element::extent (Axis a) const
{
  Dimension_cache const * d = dim_cache_[a];

  if (d->empty_b ())
    return Interval ();
  
  return d->get_dim ();
}


void
Graphical_element::do_print () const
{
#ifndef NPRINT
  DOUT << '\n';
#endif
}



void
Graphical_element::invalidate_cache (Axis a)
{
  dim_cache_[a]->invalidate ();
}

Graphical_element*
Graphical_element::parent_l (Axis a) const
{
  Dimension_cache*d= dim_cache_[a]->parent_l_;
  return d ? d->elt_l_ : 0;
}

Graphical_element::~Graphical_element ()
{
  delete dim_cache_[X_AXIS];
  delete dim_cache_[Y_AXIS];  
}

Graphical_element *
Graphical_element::common_refpoint (Link_array<Graphical_element> gs, Axis a) const
{
  Dimension_cache * common = dim_cache_[a];
  for (int i=0; i < gs.size (); i++)
    {
      common = common->common_refpoint (gs[i]->dim_cache_[a]);
    }

  return common->element_l ();
}

char const *
Graphical_element::name () const
{
  return classname (this);
}

void
Graphical_element::print () const
{
#ifndef NPRINT
  DOUT << classname (this) << "{\n";
  do_print ();
  DOUT << "}\n";
#endif
}  

void
Graphical_element::set_parent (Graphical_element *g, Axis a)
{
  dim_cache_[a]->parent_l_ = g ? g->dim_cache_[a]: 0;
}
