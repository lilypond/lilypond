/*
  graphical-element.cc -- implement Graphical_element

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "graphical-element.hh"
#include "graphical-axis-group.hh"
#include "debug.hh"

bool
Graphical_element::empty_b () const
{
  return dim_cache_[X_AXIS].empty_b () && dim_cache_[Y_AXIS].empty_b ();
}

Graphical_element::Graphical_element ()
{
  init ();
}

Graphical_element::Graphical_element (Graphical_element const &s)
  : dim_cache_ (s.dim_cache_)
{
  init ();
} 

void
Graphical_element::init ()
{
  dim_cache_[X_AXIS].elt_l_ = dim_cache_[Y_AXIS].elt_l_ = this;  
}

Real
Graphical_element::absolute_coordinate (Axis a) const
{
  return dim_cache_[a].absolute_coordinate ();
}
 

Offset
Graphical_element::absolute_offset() const
{
  return Offset (absolute_coordinate (X_AXIS), absolute_coordinate (Y_AXIS));
}



void
Graphical_element::translate_axis (Real y, Axis a)
{
  dim_cache_[a].translate (y);
}  

Real
Graphical_element::relative_coordinate (Dimension_cache*e, Axis a) const
{
  return dim_cache_[a].relative_coordinate (e);
}

Dimension_cache * 
Graphical_element::common_group (Graphical_element const* s, Axis a) const
{
  return dim_cache_[a].common_group (&s->dim_cache_[a]);
}

void
Graphical_element::translate (Offset offset)
{
  translate_axis (offset[Y_AXIS], Y_AXIS);
  translate_axis (offset[X_AXIS], X_AXIS);
}


void
Graphical_element::set_empty (bool b)
{
  dim_cache_[X_AXIS].set_empty (b);
  dim_cache_[Y_AXIS].set_empty (b);
}

Interval
Graphical_element::extent (Axis a) const
{
  Dimension_cache const * d = &dim_cache_[a];

  if (d->empty_b ())
    return Interval ();
  
  if (!d->valid_b ())
    ((Dimension_cache*)d)->set_dim  ((a == X_AXIS)? do_width(): do_height ());
  

  return d->get_dim ();
}

void
Graphical_element::unlink ()
{
  for (int a=X_AXIS; a < NO_AXES; a++)
    if (Dimension_cache * d = dim_cache_[a].parent_l_)
      {
	if (Graphical_axis_group * eg
	    = dynamic_cast<Graphical_axis_group*> (d->elt_l_))
	  eg->remove_element (this);
      }
}

void
Graphical_element::junk_links ()
{
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
  dim_cache_[a].invalidate ();
}

Graphical_element*
Graphical_element::parent_l (Axis a) const
{
  Dimension_cache*d= dim_cache_[a].parent_l_;
  return d ? d->elt_l_ : 0;
}

Graphical_element::~Graphical_element ()
{
}

Dimension_cache *
Graphical_element::common_group (Link_array<Graphical_element> gs, Axis a) const
{
  Dimension_cache * common = &dim_cache_[a];
  for (int i=0; i < gs.size (); i++)
    {
      common = common->common_group (&gs[i]->dim_cache_[a]);
    }

  return common;
}

char const *
Graphical_element::name () const
{
  return classname (this);
}
