/*   
  system-start-delimiter.cc --  implement System_start_delimiter
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2001 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include <math.h>

#include "axis-group-interface.hh"
#include "system-start-delimiter.hh"
#include "paper-def.hh"
#include "molecule.hh"
#include "font-interface.hh"
#include "all-font-metrics.hh"
#include "grob.hh"
#include "staff-symbol-referencer.hh"
#include "lookup.hh"

Molecule
System_start_delimiter::staff_bracket (Grob*me,Real height)  
{
  Real arc_height = gh_scm2double (me->get_grob_property ("arch-height")) ;
  
  SCM at = gh_list (ly_symbol2scm ("bracket"),
		    me->get_grob_property ("arch-angle"),
		    me->get_grob_property ("arch-width"),
		    gh_double2scm (arc_height),
		    gh_double2scm (height),
		    me->get_grob_property ("arch-thick"),
		    me->get_grob_property ("thickness"),
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
  Molecule mol (b, at);
  mol.align_to (X_AXIS, CENTER);
  return mol;
}

void
System_start_delimiter::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("system-start-delimiter-interface"));
}

bool
System_start_delimiter::has_interface (Grob*me)
{
  return  me->has_interface (ly_symbol2scm ("system-start-delimiter-interface"));
}

Molecule
System_start_delimiter::simple_bar (Grob*me,Real h) 
{
  Real w = me->paper_l ()->get_var ("stafflinethickness") *
    gh_scm2double (me->get_grob_property ("thickness"));
  return Lookup::filledbox (Box (Interval (0,w), Interval (-h/2, h/2)));
}

MAKE_SCHEME_CALLBACK (System_start_delimiter,after_line_breaking,1);

SCM
System_start_delimiter::after_line_breaking (SCM smob)
{
  Grob * me = unsmob_grob (smob);
  SCM   gl = me->get_grob_property ("glyph");
  
  if (scm_ilength (me->get_grob_property ("elements")) <=  1 && gl == ly_symbol2scm ("bar-line"))
    {
      me->suicide ();
    }

  return SCM_UNSPECIFIED;
}


MAKE_SCHEME_CALLBACK (System_start_delimiter,brew_molecule,1);
SCM
System_start_delimiter::brew_molecule (SCM smob)
{
  Grob * me = unsmob_grob (smob);

  SCM s = me->get_grob_property ("glyph");
  if (!gh_symbol_p (s))
    return SCM_EOL;
  
  SCM c = me->get_grob_property ("collapse-height");
  
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Interval ext = ly_scm2interval (Axis_group_interface::group_extent_callback
 (me->self_scm (), gh_int2scm (Y_AXIS)));
  Real l = ext.length () / staff_space;
  
  if (ext.empty_b ()
      || (gh_number_p (c) && l <= gh_scm2double (c)))
    {
      me->suicide ();
      return SCM_EOL;
    }

  Molecule m;
  if (s == ly_symbol2scm ("bracket"))
    m = staff_bracket (me,l);
  else if (s == ly_symbol2scm ("brace"))
    m =  staff_brace (me,l);
  else if (s == ly_symbol2scm ("bar-line"))
    m = simple_bar (me,l);
  
  m.translate_axis (ext.center (), Y_AXIS);
  return m.smobbed_copy ();
}

Molecule
System_start_delimiter::staff_brace (Grob*me, Real y)
{
  Font_metric *fm = 0;
  
  for (int i = 0; ; i++)
    {
      if (!fm || y > fm->get_char (fm->count ()-1)[Y_AXIS].length ())
	{
	  /* We go through the style sheet to lookup the font file
	     name.  This is better than using find_font directly,
	     esp. because that triggers mktextfm for non-existent
	     fonts. */
	  SCM alist = gh_list (gh_cons (ly_symbol2scm ("font-family"),
					ly_symbol2scm ("braces")),
			       gh_cons (ly_symbol2scm ("font-relative-size"),
					gh_int2scm (i)),
			       SCM_UNDEFINED);
	  fm = Font_interface::get_font (me, gh_list (alist, SCM_UNDEFINED));
	  /* Hmm, if lookup fails, we get cmr10 anyway */
	  if (ly_scm2string (gh_car (fm->description_)) == "cmr10")
	    break;
	}
      else
	break;
    }

  int lo = 0;

  int hi = (fm->count () - 1) >? 2;
  Box b;

  /* do a binary search for each Y, not very efficient, but passable?  */
  do
    {
      int cmp = (lo + hi) / 2;
      b = fm->get_char (cmp);
      if (b[Y_AXIS].empty_b () || b[Y_AXIS].length () > y)
	hi = cmp;
      else
	lo = cmp;
    }
  while (hi - lo > 1);
      
  SCM at = gh_list (ly_symbol2scm ("char"), gh_int2scm (lo), SCM_UNDEFINED);
  at = fontify_atom (fm, at);
  
  b = fm->get_char (lo);
  b[X_AXIS] = Interval (0,0);

  return Molecule (b, at);
}
  
