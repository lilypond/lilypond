/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dimension-cache.hh"
#include "span-bar.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "align-element.hh"
#include "warn.hh"

void
Span_bar::add_bar (Score_element*b)
{
  spanning_l_arr_.push (b);
  add_dependency (b);
}

void
Span_bar::do_substitute_element_pointer (Score_element*o, Score_element*n)
{
  spanning_l_arr_.unordered_substitute (o, n);
}


Interval
Span_bar::do_width () const
{
  Molecule m = lookup_l ()->bar (type_str_, 40 PT, paper_l ());
  
  return m.extent (X_AXIS);
}

void
Span_bar::do_pre_processing ()
{
  Bar::do_pre_processing ();
  
  evaluate_empty ();
  
  set_empty (false, Y_AXIS); // a hack to make mark scripts work.
}

void
Span_bar::do_post_processing ()
{
  Bar::do_post_processing ();
  Interval i(get_spanned_interval ());

  translate_axis (i.center (), Y_AXIS);
}

void
Span_bar::evaluate_empty ()
{ 
  if (spanning_l_arr_.size () < 1) 
    {
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      set_empty (true, X_AXIS, Y_AXIS);   

    }
  if (type_str_.empty_b ()) 
    {
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
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
      Graphical_element*common = common_refpoint (spanning_l_arr_[i], Y_AXIS);
      Real y = spanning_l_arr_[i]->relative_coordinate (common, Y_AXIS)  
	- relative_coordinate (common, Y_AXIS);

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
  Molecule*output = new Molecule;
  if (!iv.empty_b())
    {
      output->add_molecule (lookup_l ()->bar (type_str_, iv.length (), paper_l ()));
    }
  else
    {
      programming_error("Huh? My children deflated (FIXME)");
    }
  return output;
}



Span_bar::Span_bar ()
{
  type_str_ = "";
}
