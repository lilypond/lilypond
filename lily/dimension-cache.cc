/*   
  dimension-cache.cc --  implement Dimension_cache
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
 */

#include "dimension-cache.hh"
#include "parray.hh"
#include "score-element.hh"


Dimension_cache::Dimension_cache (Dimension_cache const &d)
{
  init();
  callback_l_ = d.callback_l_;
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
  callback_l_ =0;

  basic_offset_ =0.0;
  extra_offset_ =0.0;
  
  elt_l_ = 0;
  dim_.set_empty ();
  parent_l_ =0;
  valid_b_ = false;
  off_valid_b_ = false;
}


void
Dimension_cache::invalidate ()
{
  /*  off_valid_b_ =false;
      valid_b_ = false;*/
}


void
Dimension_cache::set_offset (Real x)
{
  // ugh!
  /*

    UGH ! UGH !
    
   */
  
  extra_offset_ = x;
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
      me->basic_offset_ += (*c) (me);
    }
  return basic_offset_ + extra_offset_;
}

Dimension_cache *
Dimension_cache::common_refpoint (Dimension_cache const* s) const
{
  Link_array<Dimension_cache> my_groups;
  for (Dimension_cache const *c = this; c ; c = c->parent_l_)
    my_groups.push ((Dimension_cache*)c);
  
  Dimension_cache const *common=0;
  
  for (Dimension_cache const * d = s; !common && d; d = d->parent_l_)
    common = (Dimension_cache const*)my_groups.find_l (d);

  return (Dimension_cache*) common;
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
  if (!callback_l_)
    {
      nc->dim_.set_empty ();
    }
  else if (!valid_b_)
    {

      nc->dim_= (*callback_l_ ) (nc);
      nc->valid_b_ = true;
    }

  r=dim_;
  return r;
}

void
Dimension_cache::set_callback (Dim_cache_callback c)
{
  callback_l_ =c;
}


