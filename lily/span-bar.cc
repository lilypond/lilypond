/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include "dimension-cache.hh"
#include "span-bar.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "align-element.hh"
#include "warn.hh"
#include "group-interface.hh"


void
Span_bar::add_bar (Score_element*b)
{
  Group_interface gi (this);
  gi.add_element (b);

  add_dependency (b);
}


Interval
Span_bar::width_callback (Dimension_cache const * c)
{
  Span_bar*  s= dynamic_cast<Span_bar*> (c->element_l ());  
  String gl = ly_scm2string (s->get_elt_property ("glyph"));

  /*urg.
   */
  Molecule m = s->compound_barline (gl, 40 PT);
  
  return m.extent (X_AXIS);
}

void
Span_bar::before_line_breaking ()
{
  Bar::before_line_breaking ();
  
  evaluate_empty ();
  
  //  set_empty (false, Y_AXIS); // a hack to make mark scripts work.
}

void
Span_bar::after_line_breaking ()
{
  Bar::after_line_breaking ();
  SCM s = get_elt_property ("collapse-height");
  if (gh_number_p (s)
      && get_spanned_interval ().length () < gh_scm2double (s))
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);
      set_empty (Y_AXIS);   
    }

  Interval i (get_spanned_interval ());
  translate_axis (i.center (), Y_AXIS);
}

void
Span_bar::evaluate_empty ()
{ 
  if (!gh_pair_p (get_elt_property ("elements")))
  {
    set_elt_property ("transparent", SCM_BOOL_T);
    set_empty (X_AXIS);
    set_empty (Y_AXIS);   
  }
  
  SCM gl = get_elt_property ("glyph");
  if (!gh_string_p (gl))
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);
      set_empty (Y_AXIS);   
    }
  else {
    String type_str = ly_scm2string (gl);
    if (type_str == "|:") 
    {
      type_str= ".|";
    }
  else if (type_str== ":|")
    {
      type_str= "|.";
    }
  else if (type_str== ":|:")
    {
      type_str= ".|.";
    }
  }

  /*
    uhh. should do something with type_str ?!!
   */
}

Interval
Span_bar::get_spanned_interval () const
{
  Interval y_int;

  for (SCM s = get_elt_property ("elements"); gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element *bar = unsmob_element ( gh_car (s));

      if (!bar)
	continue;
      
      Score_element*common = common_refpoint (bar, Y_AXIS);

      Interval iv (bar->extent(Y_AXIS));
      if (!iv.empty_b ())
	{
	  Real y = bar->relative_coordinate (common, Y_AXIS)  
	    - relative_coordinate (common, Y_AXIS);

	  y_int.unite (y + iv);
	}
    }
  
  return y_int;
}

Interval
Span_bar::height_callback (Dimension_cache const *c) 
{
  Span_bar * s= dynamic_cast<Span_bar*> (c->element_l ()); 
  return s->get_spanned_interval ();
}

Real
Span_bar::get_bar_size () const
{
   Interval iv (get_spanned_interval ());
   if (iv.empty_b ())
     {
       programming_error("Huh? My children deflated (FIXME)");
       iv = Interval (0,0);
     }
   return iv.length ();
}

Span_bar::Span_bar ()
{
  group (this).set_interface ();
  dim_cache_[X_AXIS]->set_callback (width_callback);
  dim_cache_[Y_AXIS]->set_callback (height_callback);  
}

