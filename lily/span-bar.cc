/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dimen.hh"
#include "span-bar.hh"
#include "lookup.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "vertical-align-elem.hh"

void
Span_bar::add (Bar*b)
{
  spanning_l_arr_.push (b);
  add_dependency (b);
}



void
Span_bar::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  spanning_l_arr_.unordered_substitute (o, n);
}


void
Span_bar::set (Vertical_align_element *a)
{
  add_dependency (a);
}
  

Interval
Span_bar::do_width() const
{
  return paper()->lookup_l ()->bar (type_str_, 40 PT).dim_.x (); // ugh
}

void
Span_bar::do_pre_processing()
{
  Bar::do_pre_processing ();
  
  if (spanning_l_arr_.size() < 1) 
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
}

Atom
Span_bar::get_bar_sym (Real dy) const
{
  return paper()->lookup_l ()->bar (type_str_, dy);
}


Molecule*
Span_bar::brew_molecule_p() const
{
  Interval y_int;
  for (int i=0; i < spanning_l_arr_.size(); i++) 
    {
      Axis_group_element *common = 
	common_group (spanning_l_arr_[i], Y_AXIS);
	
      Real y = spanning_l_arr_[i]->relative_coordinate (common, Y_AXIS)  
	-relative_coordinate (common,Y_AXIS);

      y_int.unite (y + spanning_l_arr_[i]->height());
    }

  Atom s = get_bar_sym (y_int.length());
  Molecule*output = new Molecule (Atom (s));
  output->translate_axis (y_int.center(), Y_AXIS);
  return output;
}


IMPLEMENT_IS_TYPE_B1(Span_bar,Bar);

Span_bar::Span_bar()
{
  type_str_ = "";
}
