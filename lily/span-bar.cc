/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "dimen.hh"
#include "span-bar.hh"
#include "lookup.hh"
#include "symbol.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "vertical-align-elem.hh"

void
Span_bar::add (Bar*b)
{
  b->spanned_i_ ++;
  spanning_l_arr_.push (b);
  add_dependency (b);
}



void
Span_bar::do_substitute_dependency (Score_elem*o, Score_elem*n)
{
  Bar * bold = 0;
  if  (o->is_type_b (Bar::static_name())) 
	bold = (Bar*)o->item();
  else
	return;

  bold->spanned_i_ --;
  Bar * b =0;
  if (n && n->is_type_b (Bar::static_name())) 
    {
	b = (Bar*)n->item();
	b->spanned_i_ ++;
    }
  
  spanning_l_arr_.substitute (bold , b);
}


void
Span_bar::set (Vertical_align_element *a)
{
  add_dependency (a);
}
  

Interval
Span_bar::do_width() const
{
  return paper()->lookup_l ()->bar (type_str_, 40 PT).dim.x (); // ugh
}
void
Span_bar::do_pre_processing()
{
  if ( spanning_l_arr_.size() < 1) 
    {
	transparent_b_ = true;
	empty_b_ =true;
    }
  else 
    {
	if (type_str_ == "")
	    type_str_ = spanning_l_arr_[0]->type_str_;
	if (type_str_ =="") 
	  {
	    transparent_b_=true;
	    empty_b_ = true;
	  }
	else if ( type_str_ == "|:") 
	  {
	    type_str_ = ".|";
	  }
	else if ( type_str_ == ":|")
	  {
	    type_str_ = "|.";
	  }
    }
}

Symbol
Span_bar::get_bar_sym (Real dy) const
{
  return paper()->lookup_l ()->bar (type_str_, dy);
}


Molecule*
Span_bar::brew_molecule_p()const
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

  Symbol s = get_bar_sym (y_int.length());
  Molecule*output = new Molecule (Atom (s));
  output->translate (  y_int[-1], Y_AXIS);
  return output;
}


IMPLEMENT_IS_TYPE_B1(Span_bar,Bar);

Span_bar::Span_bar()
{
  type_str_ = "";
}
