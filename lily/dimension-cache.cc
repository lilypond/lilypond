/*   
  dimension-cache.cc --  implement Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "dimension-cache.hh"
#include "parray.hh"

Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init();
  empty_b_ = d.empty_b_;
  offset_ = d.offset_; //let's hope others will copy  the refpoint appropriately. 
}

Dimension_cache::Dimension_cache ()
{
  init();
}

void
Dimension_cache::init()
{
  offset_ =0.0;
  elt_l_ = 0;
  dim_.set_empty ();
  parent_l_ =0;
  valid_b_ = false;
  empty_b_ = false;
}


void
Dimension_cache::invalidate ()
{
  valid_b_ = false;
  invalidate_dependencies ();
}

void
Dimension_cache::invalidate_dependencies ()
{
  for (int i=0; i < dependencies_l_arr_.size (); i++)
    {
      Dimension_cache * g = dependencies_l_arr_[i];
      if (g->valid_b_)
	{
	  g->invalidate ();
	}
    }
}

void
Dimension_cache::set_offset (Real x)
{
  invalidate_dependencies ();
  offset_ = x;
}

void
Dimension_cache::translate (Real x)
{
  invalidate_dependencies ();
  offset_ += x;
}


Real
Dimension_cache::absolute_coordinate () const
{
  Real r = offset_;
  for (Dimension_cache * c = parent_l_;
       c; c = c->parent_l_)
    r += c->offset_;
  return r;
}

/*
  what *should* these functions *do* anyway.
 */
Real
Dimension_cache::relative_coordinate (Dimension_cache *d) const
{
  Real r =0.0;
  if (d == this)		// UGH
    return 0.0;

  for (Dimension_cache* c = parent_l_;
       c != d;
       c = c->parent_l_)
    r +=  c->offset_;
  return r;
}

Dimension_cache *
Dimension_cache::common_group (Dimension_cache const* s) const
{
  Link_array<Dimension_cache const> my_groups;
  for (Dimension_cache const *c = this;
       c ; c = c->parent_l_)
    my_groups.push (c);
  
  
  Dimension_cache const *common=0;
  
  for (Dimension_cache const * d = s;
       !common && d;
       d = d->parent_l_)
    common = my_groups.find_l (d);

  return (Dimension_cache*)common;
}



void
Dimension_cache::set_empty (bool b)
{
  if (empty_b_ != b)
    {
      empty_b_ = b;
      if (!empty_b_)
	invalidate ();
    }
}  

void
Dimension_cache::set_dim (Interval v)
{
  dim_ = v;
  valid_b_ = true;
}
  

Interval
Dimension_cache::get_dim () const
{
  Interval r;
  if (empty_b_)
    {
      r.set_empty ();
      return r;
    }
      
  assert (valid_b_);

  r=dim_;
  if (!r.empty_b()) // float exception on DEC Alpha
    r += offset_;

  return r;
}


