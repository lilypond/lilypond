/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "lookup.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "warn.hh"
#include "axis-group-interface.hh"
#include "group-interface.hh"
#include "score-element.hh"
#include "bar.hh"

void
Span_bar::add_bar (Score_element*me, Score_element*b)
{
  Pointer_group_interface gi (me);
  gi.add_element (b);

  me->add_dependency (b);
}

Interval
Span_bar::width_callback (Score_element *se, Axis )
{
  String gl = ly_scm2string (se->get_elt_property ("glyph"));

  /*
    urg.
   */
  Molecule m = Bar::compound_barline (se, gl, 40 PT);
  
  return m.extent (X_AXIS);
}

MAKE_SCHEME_CALLBACK(Span_bar,before_line_breaking);
SCM
Span_bar::before_line_breaking (SCM smob)
{
  Bar::before_line_breaking (smob);
  
  evaluate_empty (unsmob_element (smob));

  return SCM_UNSPECIFIED;
}

Real
Span_bar::center_on_spanned_callback (Score_element * me, Axis a)
{
  assert (a == Y_AXIS);
  Interval i (get_spanned_interval (me));

  /*
    Bar::brew_molecule delivers a barline of y-extent (-h/2,h/2), so
    we have to translate ourselves to be in the center of the 
    interval that we span.  */

  return i.center ();
}

void
Span_bar::evaluate_empty (Score_element*me)
{
  /*
    TODO: filter all hara-kiried out of ELEMENS list, and then
    optionally do suicide. Call this cleanage function from
    center_on_spanned_callback() as well.
    
   */
  if (!gh_pair_p (me->get_elt_property ("elements")))
    {
      me->suicide ();
    }
  
  SCM gl = me->get_elt_property ("glyph");
  if (!gh_string_p (gl))
    {
      me->suicide ();
      return ; 
    }
  else {
    String type_str = ly_scm2string (gl);
    String orig = type_str;
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
    if (orig != type_str)
      me->set_elt_property ("glyph", ly_str02scm (type_str.ch_C()));
  }
}

Interval
Span_bar::get_spanned_interval (Score_element*me) 
{
  return Axis_group_interface::group_extent_callback (me, Y_AXIS);  
}


MAKE_SCHEME_CALLBACK(Span_bar,get_bar_size);
SCM
Span_bar::get_bar_size (SCM smob)
{
  Score_element* me =  unsmob_element (smob);
  Interval iv (get_spanned_interval (me));
  if (iv.empty_b ())
    {
      /*
	This happens if the bars are hara-kiried from under us.
       */
      me->suicide ();
      return gh_double2scm (-1);
    }
  return gh_double2scm (iv.length ());
}
void
Span_bar::set_interface (Score_element *me)
{
  Bar::set_interface (me);
  
  Pointer_group_interface(me).set_interface ();
  me->set_extent_callback (width_callback, X_AXIS);
  me->add_offset_callback (center_on_spanned_callback, Y_AXIS);
  me->set_interface (ly_symbol2scm ("span-bar-interface"));
  me->set_extent_callback (0, Y_AXIS);
}

bool
Span_bar::has_interface (Score_element*m)
{
  return m && m->has_interface (ly_symbol2scm ("span-bar-interface"));
}
