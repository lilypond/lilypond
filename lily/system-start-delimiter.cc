/*   
  system-start-delimiter.cc --  implement System_start_delimiter
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>

#include "axis-group-interface.hh"
#include "system-start-delimiter.hh"
#include "paper-def.hh"
#include "stencil.hh"
#include "font-interface.hh"
#include "all-font-metrics.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"

Stencil
System_start_delimiter::staff_bracket (Grob*me,Real height)  
{
  Real arc_height = ly_scm2double (me->get_property ("arch-height")) ;
  
  SCM at = scm_list_n (ly_symbol2scm ("bracket"),
		    me->get_property ("arch-angle"),
		    me->get_property ("arch-width"),
		    scm_make_real (arc_height),
		    scm_make_real (height),
		    me->get_property ("arch-thick"),
		    me->get_property ("thickness"),
		    SCM_UNDEFINED);

/*
TODO: sort this out.
    
Another thing:
In system-start-delimiter.cc I see the line

  Real h = height + 2 * arc_height;

But I really think that you mean

 Real h = height + 2 * arc_width;

(arc_height changes the x-axis-size of arc ; arc_width changes the
y-axis-size)
Will not fix it since I'm not sure.
  
   */

  Real h = height + 2 * arc_height;
  Box b (Interval (0, 1.5), Interval (-h/2, h/2));
  Stencil mol (b, at);
  mol.align_to (X_AXIS, CENTER);
  return mol;
}



Stencil
System_start_delimiter::simple_bar (Grob*me,Real h) 
{
  Real lt =me->get_paper ()->get_dimension (ly_symbol2scm ("linethickness")) ;
  Real w = lt * robust_scm2double (me->get_property ("thickness"), 1);
  return Lookup::round_filled_box (Box (Interval (0,w), Interval (-h/2, h/2)),
				   lt);
}

MAKE_SCHEME_CALLBACK (System_start_delimiter,after_line_breaking,1);

SCM
System_start_delimiter::after_line_breaking (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  SCM   gl = me->get_property ("glyph");
  if (ly_c_equal_p (gl,scm_makfrom0str ("bar-line")))
    {
      int count = 0;

      /*
	Get all coordinates, to trigger Hara kiri. 
      */
      SCM elts = me->get_property ("elements");
      Grob *common = common_refpoint_of_list (elts, me, Y_AXIS);
      for (SCM s = elts; ly_c_pair_p (s); s = ly_cdr (s))
	{
	  Interval v = unsmob_grob (ly_car (s))->extent (common, Y_AXIS);

	  if (!v.is_empty ())
	    count ++;
	}
  
  
      if (count <=  1)
	{
	  me->suicide ();
	}
    }
  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (System_start_delimiter,print,1);
SCM
System_start_delimiter::print (SCM smob)
{
  Grob * me = unsmob_grob (smob);

  SCM s = me->get_property ("glyph");
  if (!ly_c_string_p (s))
    return SCM_EOL;
  SCM gsym = scm_string_to_symbol (s) ;
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Interval ext = ly_scm2interval (Axis_group_interface::group_extent_callback
 (me->self_scm (), scm_int2num (Y_AXIS)));
  Real l = ext.length () / staff_space;
  
  if (ext.is_empty ()
      || (robust_scm2double (me->get_property ("collapse-height"), 0.0) >= l))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Stencil m;

  if (gsym== ly_symbol2scm ("bracket"))
    m = staff_bracket (me,l);
  else if (gsym == ly_symbol2scm ("brace"))
    m =  staff_brace (me,l);
  else if (gsym == ly_symbol2scm ("bar-line"))
    m = simple_bar (me,l);
  
  m.translate_axis (ext.center (), Y_AXIS);
  return m.smobbed_copy ();
}

Stencil
System_start_delimiter::staff_brace (Grob*me, Real y)
{
  Font_metric *fm = 0;
  
  /* We go through the style sheet to lookup the font file
     name.  This is better than using find_font directly,
     esp. because that triggers mktextfm for non-existent
     fonts. */
  SCM fam = scm_cons (ly_symbol2scm ("font-encoding"), ly_symbol2scm ("fetaBraces"));
  
  SCM alist = scm_list_n (fam, SCM_UNDEFINED);
  fm = select_font (me->get_paper (), scm_list_n (alist, SCM_UNDEFINED));
  

  int lo = 0;

  int hi = (fm->count () - 1) >? 2;
  Box b;

  /* do a binary search for each Y, not very efficient, but passable?  */
  do
    {
      int cmp = (lo + hi) / 2;
      b = fm->get_indexed_char (cmp);
      if (b[Y_AXIS].is_empty () || b[Y_AXIS].length () > y)
	hi = cmp;
      else
	lo = cmp;
    }
  while (hi - lo > 1);

  /* FIXME: ascii? */
  Stencil stil (fm->get_indexed_char_stencil (lo));
  b = stil.extent_box ();
  b[X_AXIS] = Interval (0, 0);

  return Stencil (b, stil.expr ());
}
  



ADD_INTERFACE (System_start_delimiter,"system-start-delimiter-interface",
	       "The brace, bracket or bar in front of the system. "
	       "It is implemented as a spanner."
	       ,
	       "collapse-height thickness "
	       "arch-height arch-angle arch-thick arch-width bracket-thick glyph");
