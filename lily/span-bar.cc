/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "span-bar.hh"
#include "font-interface.hh"
#include "dimensions.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "warn.hh"
#include "axis-group-interface.hh"
#include "group-interface.hh"
#include "grob.hh"
#include "bar-line.hh"

void
Span_bar::add_bar (Grob*me, Grob*b)
{
  Pointer_group_interface::add_grob (me, ly_symbol2scm ("elements"),  b);

  me->add_dependency (b);
}

MAKE_SCHEME_CALLBACK (Span_bar,brew_molecule,1);

/*
  Limitations/Bugs:

   (1) Elements from 'me->get_grob_property ("elements")' must be
   ordered according to their y coordinates relative to their common
   axis group parent.  Otherwise, the computation goes mad.

   (TODO:
   apply a sort algorithm that ensures this precondition.)  However,
   until now, I have seen no case where lily has not fulfilled this
   precondition.

   (2) This method depends on bar_engraver not being removed from
   staff context.  If bar_engraver is removed, the size of the staff
   lines is evaluated as 0, which results in a solid span bar line
   with faulty y coordinate.
 */

/*
  This routine was originally by Juergen Reuter, but it was a on the
  bulky side. Rewritten by Han-Wen. 
 */
SCM
Span_bar::brew_molecule (SCM smobbed_me) 
{
  Grob *me = unsmob_grob (smobbed_me);
  SCM first_elt = me->get_grob_property ("elements");

  // compute common refpoint of elements
  Grob *refp = me;
  for (SCM elts = first_elt; gh_pair_p (elts); elts = ly_cdr (elts))
    {
      SCM smobbed_staff_bar = ly_car (elts);
      Grob *staff_bar = unsmob_grob (smobbed_staff_bar);
      refp = staff_bar->common_refpoint (refp, Y_AXIS);
    }

  Span_bar::evaluate_glyph(me);
  SCM glyph = me->get_grob_property ("glyph");

  /*
    glyph may not be a string, when ME is killed by Hara Kiri in
    between.
  */
  if (!gh_string_p (glyph))
    return SCM_EOL;
  
  String glyph_str = ly_scm2string (glyph);

  // compose span_bar_mol
  Molecule span_bar_mol;

  Interval prev_extent;
  for (SCM elts = first_elt; gh_pair_p (elts); elts = ly_cdr (elts))
    {
      SCM smobbed_staff_bar = ly_car (elts);
      Grob *staff_bar = unsmob_grob (smobbed_staff_bar);
      Interval ext = staff_bar->extent (refp, Y_AXIS);
      if (ext.empty_b ())
	continue; 
      
      if (!prev_extent.empty_b ())
	{
	  Interval l(prev_extent [UP],
		     ext[DOWN]);

	  if (l.empty_b ())
	    {
	      /* there is  overlap between the bar lines. We do nothign here.
	       */
	    }
	  else
	    {
	      Molecule interbar
		= Bar_line::compound_barline (staff_bar, glyph_str, l.length());
	      interbar.translate_axis (l.center (), Y_AXIS);
	      span_bar_mol.add_molecule (interbar);
	    }
	}
      prev_extent = ext;
    }

  span_bar_mol.translate_axis (- me->relative_coordinate (refp, Y_AXIS), Y_AXIS);
  
  return span_bar_mol.smobbed_copy ();
}

MAKE_SCHEME_CALLBACK (Span_bar,width_callback,2);
SCM
Span_bar::width_callback (SCM element_smob, SCM scm_axis)
{
  Grob *se = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (scm_axis);
  assert (a == X_AXIS);
  String gl = ly_scm2string (se->get_grob_property ("glyph"));

  /*
    urg.
  */
  Molecule m = Bar_line::compound_barline (se, gl, 40 PT);
  
  return ly_interval2scm (m.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Span_bar,before_line_breaking,1);
SCM
Span_bar::before_line_breaking (SCM smob)
{
  evaluate_empty (unsmob_grob (smob));
  evaluate_glyph (unsmob_grob (smob));

  /*
    no need to call   Bar_line::before_line_breaking (), because the info
    in ELEMENTS already has been procced by Bar_line::before_line_breaking ().
  */
  return SCM_UNSPECIFIED;
}

MAKE_SCHEME_CALLBACK (Span_bar,center_on_spanned_callback,2);

SCM
Span_bar::center_on_spanned_callback (SCM element_smob, SCM axis)
{
  Grob *me = unsmob_grob (element_smob);
  Axis a = (Axis) gh_scm2int (axis);
  assert (a == Y_AXIS);
  Interval i (get_spanned_interval (me));

  /*
    Bar_line::brew_molecule delivers a barline of y-extent (-h/2,h/2), so
    we have to translate ourselves to be in the center of the 
    interval that we span.  */
  if (i.empty_b ())
    {
      me->suicide ();
      return gh_double2scm (0.0);
    }
  
  return gh_double2scm (i.center ());
}

void
Span_bar::evaluate_empty (Grob*me)
{
  /*
    TODO: filter all hara-kiried out of ELEMENS list, and then
    optionally do suicide. Call this cleanage function from
    center_on_spanned_callback () as well.
    
   */
  if (!gh_pair_p (me->get_grob_property ("elements")))
    {
      me->suicide ();
    }
}

void
Span_bar::evaluate_glyph (Grob*me)
{
  SCM elts = me->get_grob_property ("elements");
  SCM glyph_symbol = ly_symbol2scm ("glyph");
  SCM gl = SCM_EOL;

  while (gh_pair_p (elts))
    {
      gl =  unsmob_grob (gh_car (elts))->internal_get_grob_property (glyph_symbol);
      if (gh_string_p (gl))
	break;
      elts =gh_cdr (elts);
    }

  if (!gh_string_p (gl))
    {
      me->suicide ();
      return;
    }
  
  String type = ly_scm2string (gl);
  if (type == "|:") 
    {
      type = ".|";
    }
  else if (type== ":|")
    {
      type = "|.";
    }
  else if (type== ":|:")
    {
      type = ".|.";
    }

  gl = ly_str02scm (type.ch_C ());
  if (scm_equal_p (me->internal_get_grob_property (glyph_symbol), gl) != SCM_BOOL_T)
    me->internal_set_grob_property (glyph_symbol, gl);
}

Interval
Span_bar::get_spanned_interval (Grob*me) 
{
  return ly_scm2interval (Axis_group_interface::group_extent_callback (me->self_scm (), gh_int2scm (Y_AXIS))); 
}


MAKE_SCHEME_CALLBACK (Span_bar,get_bar_size,1);
SCM
Span_bar::get_bar_size (SCM smob)
{
  Grob* me =  unsmob_grob (smob);
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


bool
Span_bar::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("span-bar-interface"));
}

ADD_INTERFACE (Span_bar,"span-bar-interface",
  "A bar line that spans other barlines (typically used to get cross-staff barlines.",
  "");


