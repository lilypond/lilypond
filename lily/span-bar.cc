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
  
  Molecule m = s->lookup_l ()->bar (gl, 40 PT, s->paper_l ());
  
  return m.extent (X_AXIS);
}

void
Span_bar::do_pre_processing ()
{
  Bar::do_pre_processing ();
  
  evaluate_empty ();
  
  //  set_empty (false, Y_AXIS); // a hack to make mark scripts work.
}

void
Span_bar::do_post_processing ()
{
  Bar::do_post_processing ();
  Interval i(get_spanned_interval ());

  translate_axis (i.center (), Y_AXIS);

  /*
    {[ stacking shift
    ugh
  */
  SCM me = get_elt_property ("glyph");
  if (gh_string_p (me) && ly_scm2string (me) == "bracket")
    {
      SCM e = get_elt_property ("other");
      if (e != SCM_UNDEFINED)
	{
	  assert (SMOB_IS_TYPE_B (Score_element, e));
	  Score_element* se = SMOB_TO_TYPE (Score_element, e);
	  SCM her = se->get_elt_property ("glyph");
#if 0
	  // urg x-extent broken?
	  if (gh_string_p (her) && ly_scm2string (her) == "brace")
	    se->translate_axis (-extent (X_AXIS).length (), X_AXIS);
#else
	  if (gh_string_p (her) && ly_scm2string (her) == "brace"
	      && !to_boolean (get_elt_property ("transparent")))
	    se->translate_axis (-paper_l ()->get_var ("interline"), X_AXIS);
#endif 
	}
    }
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

Molecule*
Span_bar::do_brew_molecule_p () const
{
  Interval iv (get_spanned_interval ());
  Molecule*output = new Molecule;
  SCM s = get_elt_property ("glyph");
  if (gh_string_p (s) && !iv.empty_b())
    {
      output->add_molecule (lookup_l ()->bar (ly_scm2string (s),
					      iv.length (),
					      paper_l ()));
    }
  else
    {
      programming_error("Huh? My children deflated (FIXME)");
    }
  return output;
}



Span_bar::Span_bar ()
{
  dim_cache_[X_AXIS]->set_callback (width_callback);
  dim_cache_[Y_AXIS]->set_callback (height_callback);  
}

