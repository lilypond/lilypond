/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "lookup.hh"
#include "dimensions.hh"

#include "paper-def.hh"
#include "molecule.hh"
#include "align-element.hh"

void
Span_bar::add_bar (Bar*b)
{
  spanning_l_arr_.push (b);
  add_dependency (b);
}

void
Span_bar::do_substitute_dependency (Score_element*o, Score_element*n)
{
  spanning_l_arr_.unordered_substitute (o, n);
}

void
Span_bar::set_align (Align_element *a)
{
  add_dependency (a);
}

Interval
Span_bar::do_width () const
{
  if (no_width_b_)
    {
      return Interval (0,0);
    }
  
  Molecule m = lookup_l ()->bar (type_str_, 40 PT);
  
  return m.extent (X_AXIS);
}

void
Span_bar::do_pre_processing ()
{
  Bar::do_pre_processing ();
  
  evaluate_empty ();
  translate_axis (extra_x_off_, X_AXIS);
  
  dim_cache_[Y_AXIS].set_empty (false); // a hack to make mark scripts work.
}

void
Span_bar::do_post_processing ()
{
  Bar::do_post_processing ();
}

void
Span_bar::evaluate_empty ()
{ 
  if (spanning_l_arr_.size () < 1) 
    {
      transparent_b_ = true;
      set_empty (true);   
    }
  if (type_str_.empty_b ()) 
    {
      transparent_b_=true;
      set_empty (true);
    }
  else if (type_str_ == "|:") 
    {
      type_str_ = ".|";
    }
  else if (type_str_ == ":|")
    {
      type_str_ = "|.";
    }
  else if (type_str_ == ":|:")
    {
      type_str_ = ".|.";
    }
}

Interval
Span_bar::get_spanned_interval () const
{
  Interval y_int;
  for (int i=0; i < spanning_l_arr_.size (); i++) 
    {
      Dimension_cache*common = 
	common_group (spanning_l_arr_[i], Y_AXIS);
	
      Real y = spanning_l_arr_[i]->dim_cache_[Y_AXIS].relative_coordinate (common)  
	-dim_cache_[Y_AXIS].relative_coordinate (common);

      y_int.unite (y + spanning_l_arr_[i]->extent(Y_AXIS));
    }
  return y_int;
}

Interval
Span_bar::do_height () const
{
  return get_spanned_interval ();
}

Molecule*
Span_bar::do_brew_molecule_p () const
{
  Interval iv (get_spanned_interval ());
  Molecule*output = new Molecule (lookup_l ()->bar (type_str_, iv.length ()));

  output->translate_axis (iv.center (), Y_AXIS);
  return output;
}



Span_bar::Span_bar ()
{
  type_str_ = "";
  extra_x_off_ = 0.0;
  no_width_b_ = false;
}
