/*
  span-bar.cc -- implement Span_bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
#include "bar.hh"

void
Span_bar::add_bar (Grob*me, Grob*b)
{
  Pointer_group_interface::add_element (me,"elements", b);

  me->add_dependency (b);
}

MAKE_SCHEME_CALLBACK (Span_bar,brew_molecule,1);

/**
 * Limitations/Bugs:
 *
 * (1) Elements from 'me->get_grob_property ("elements")' must be
 * ordered according to their y coordinates relative to their common
 * axis group parent.  Otherwise, the computation goes mad.  (TODO:
 * apply a sort algorithm that ensures this precondition.)  However,
 * until now, I have seen no case where lily has not fulfilled this
 * precondition.
 *
 * (2) This method depends on bar_engraver not being removed from
 * staff context.  If bar_engraver is removed, the size of the staff
 * lines is evaluated as 0, which results in a solid span bar line
 * with faulty y coordinate.
 *
 */
SCM
Span_bar::brew_molecule (SCM smobbed_me) 
{
  Grob *me = unsmob_grob (smobbed_me);
  Span_bar::evaluate_glyph(me);
  SCM glyph = me->get_grob_property (ly_symbol2scm ("glyph"));
  String glyph_str = ly_scm2string (glyph);
  SCM first_elt = me->get_grob_property ("elements");

  // first walk: compute axis_group parent via common_refpoint () on all bars
  Grob *refpoint = 0;
  int staff_bar_count = 0;
  for (SCM elts = first_elt;
       elts != SCM_EOL;
       elts = gh_cdr (elts))
  {
    SCM smobbed_staff_bar = gh_car (elts);
    Grob *staff_bar = unsmob_grob (smobbed_staff_bar);
    refpoint = (staff_bar_count > 0) ?
      staff_bar->common_refpoint (refpoint, Y_AXIS) :
      staff_bar;
    staff_bar_count++;
  }
  /* assert: refpoint is an axis-group object */
  Grob *axis_group = refpoint;

  // second walk: collect span bar components;
  // compute extent of axis_group
  Real last_staff_bar_length;
  Real *interstaff_bar_length = new Real[staff_bar_count];
  Real *interstaff_bar_yoffs = new Real[staff_bar_count];
  Molecule *interstaff_bar_molecule = new Molecule[staff_bar_count];
  Real axis_group_extent = 0.0;
  staff_bar_count = 0;
  for (SCM elts = first_elt;
       elts != SCM_EOL;
       elts = gh_cdr (elts))
  {
    SCM smobbed_staff_bar = gh_car (elts);
    SCM smobbed_staff_bar_molecule =
      Bar::brew_molecule (smobbed_staff_bar);
    Grob *staff_bar = unsmob_grob (smobbed_staff_bar);
    interstaff_bar_yoffs[staff_bar_count] =
      staff_bar->relative_coordinate (axis_group, (Axis)Y_AXIS);
    if (smobbed_staff_bar_molecule != SCM_EOL)
    {
      Real staff_bar_length =
	unsmob_molecule (smobbed_staff_bar_molecule)->
	extent (Y_AXIS).length ();
      if (staff_bar_count > 0)
      {
	// clone bar_molecule and fix y extent
	interstaff_bar_length[staff_bar_count] =
	  interstaff_bar_yoffs[staff_bar_count] -
	  interstaff_bar_yoffs[staff_bar_count - 1] -
	  last_staff_bar_length;
	SCM smobbed_interstaff_bar_molecule = 
	  Bar::compound_barline (staff_bar, glyph_str,
				 interstaff_bar_length[staff_bar_count]).
	  smobbed_copy ();
	interstaff_bar_molecule[staff_bar_count] =
	  *unsmob_molecule (smobbed_interstaff_bar_molecule);
      }
      else
      {
	interstaff_bar_molecule[staff_bar_count] = Molecule::Molecule ();
      }
      last_staff_bar_length = staff_bar_length;
    }
    else
    {
      last_staff_bar_length = 0;
      interstaff_bar_length[staff_bar_count] = 0;
      interstaff_bar_molecule[staff_bar_count] = Molecule::Molecule ();
    }
    axis_group_extent += last_staff_bar_length;
    axis_group_extent += interstaff_bar_length[staff_bar_count];
    staff_bar_count++;
  }
  // assert(abs(axis_group_extent -
  //		axis_group->extent (axis_group, Y_AXIS).length ()) < EPSILON);

  // third walk: correct y axis on all span bar components;
  // put all components into a single span bar molecule
  Molecule span_bar_molecule = Molecule::Molecule ();
  staff_bar_count = 0;
  for (SCM elts = first_elt;
       elts != SCM_EOL;
       elts = gh_cdr (elts))
  {
    interstaff_bar_yoffs[staff_bar_count] +=
      (axis_group_extent - interstaff_bar_length[staff_bar_count]) / 2;
    interstaff_bar_molecule[staff_bar_count].//DEBUG
      translate_axis (-1.0 PT, X_AXIS);//DEBUG
    interstaff_bar_molecule[staff_bar_count].
      translate_axis (interstaff_bar_yoffs[staff_bar_count], Y_AXIS);
    span_bar_molecule.add_molecule (interstaff_bar_molecule[staff_bar_count]);
    staff_bar_count++;
  }

  // clean-up & exit
  delete interstaff_bar_length;
  delete interstaff_bar_yoffs;
  delete interstaff_bar_molecule;
  return span_bar_molecule.smobbed_copy ();
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
  Molecule m = Bar::compound_barline (se, gl, 40 PT);
  
  return ly_interval2scm (m.extent (X_AXIS));
}

MAKE_SCHEME_CALLBACK (Span_bar,before_line_breaking,1);
SCM
Span_bar::before_line_breaking (SCM smob)
{
  evaluate_empty (unsmob_grob (smob));
  evaluate_glyph (unsmob_grob (smob));

  /*
    no need to call   Bar::before_line_breaking (), because the info
    in ELEMENTS already has been procced by Bar::before_line_breaking ().
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
    Bar::brew_molecule delivers a barline of y-extent (-h/2,h/2), so
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
  Grob * b = unsmob_grob (gh_car (elts));
  SCM glsym =ly_symbol2scm ("glyph");
  SCM gl =b ->get_grob_property (glsym);
  if (!gh_string_p (gl))
    {
      me->suicide ();
      return ; 
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
  if (scm_equal_p (me->get_grob_property (glsym), gl) != SCM_BOOL_T)
    me->set_grob_property (glsym, gl);
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

void
Span_bar::set_interface (Grob *me)
{
  Bar::set_interface (me);
  
  me->set_interface (ly_symbol2scm ("span-bar-interface"));
  me->set_extent_callback (SCM_EOL, Y_AXIS);
}

bool
Span_bar::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("span-bar-interface"));
}
