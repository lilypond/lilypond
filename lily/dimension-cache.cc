/*   
  dimension-cache.cc --  implement Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */
#include <math.h>
#include "warn.hh"

#include "dimension-cache.hh"
#include "parray.hh"
#include "score-element.hh"


Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init();
  extent_callback_l_ = d.extent_callback_l_;
  basic_offset_ = d.basic_offset_;
  extra_offset_ = d.extra_offset_;
  off_valid_b_ = d.off_valid_b_;
  off_callbacks_ = d.off_callbacks_;
  parent_l_ = d.parent_l_;  
}

Dimension_cache::Dimension_cache ()
{
  init();
}

void
Dimension_cache::init()
{
  extent_callback_l_ =0;
  basic_offset_ =0.0;
  extra_offset_ =0.0;
  
  elt_l_ = 0;
  dim_.set_empty ();
  parent_l_ =0;
  valid_b_ = false;
  off_valid_b_ = false;
}


void
Dimension_cache::translate (Real x)
{
  extra_offset_ += x;
}

Real
Dimension_cache::relative_coordinate (Dimension_cache *refp) const
{
  if (refp == this)
    return 0.0;

  /*
    We catch PARENT_L_ == nil case with this, but we crash if we did
    not ask for the absolute coordinate (ie. REFP == nil.)
    
   */
  if (refp == parent_l_)
    return get_offset ();
  else
    return get_offset () + parent_l_->relative_coordinate (refp);
}

Axis
Dimension_cache::axis () const
{
  if (elt_l_-> dim_cache_[X_AXIS] == this)
    return X_AXIS;
  else
    return Y_AXIS;
}

Real
Dimension_cache::get_offset () const
{
  Dimension_cache *me = (Dimension_cache*) this;
  while (off_callbacks_.size ())
    {
      Offset_cache_callback c = me->off_callbacks_[0];
      me->off_callbacks_.del (0);
      Real r =  (*c) (me);
      if (isinf (r) || isnan (r))
	{
	  r = 0.0;
	  programming_error ("Infinity or NaN encountered");
	}
      me->basic_offset_ +=r;
    }
  return basic_offset_ + extra_offset_;
}

Dimension_cache *
Dimension_cache::common_refpoint (Dimension_cache const* s) const
{
  /*
    I don't like the quadratic aspect of this code. Maybe this should
    be rewritten some time, but the largest chain of parents might be
    10 high or so, so it shouldn't be a real issue. */
  for (Dimension_cache const *c = this; c; c = c->parent_l_)
    for (Dimension_cache const * d = s; d; d = d->parent_l_)
      if (d == c)
	return (Dimension_cache*)d;

  return 0;
}

Interval
Dimension_cache::point_dimension_callback (Dimension_cache const* )
{
  return Interval (0,0);
}

Interval
Dimension_cache::get_dim () const
{
  Interval r;
  Dimension_cache *nc = ((Dimension_cache*)this);
  if (!extent_callback_l_)
    {
      nc->dim_.set_empty ();
    }
  else if (!valid_b_)
    {
      nc->dim_= (*extent_callback_l_ ) (nc);
      nc->valid_b_ = true;
    }

  r=dim_;
  return r;
}

void
Dimension_cache::set_callback (Dim_cache_callback c)
{
  extent_callback_l_ =c;
}


