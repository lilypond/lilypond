/*
  bar.cc -- implement Bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

#include "lookup.hh"
#include "paper-column.hh"
#include "main.hh"
#include "grob.hh"
#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "font-interface.hh"
#include "debug.hh"
#include "all-font-metrics.hh"
#include "item.hh"
#include "staff-symbol-referencer.hh"

MAKE_SCHEME_CALLBACK (Bar,brew_molecule,1);

SCM 
Bar::brew_molecule (SCM smob) 
{
  Grob * me = unsmob_grob (smob);

  SCM s = me->get_grob_property ("glyph");
  SCM barsiz_proc = me->get_grob_property ("bar-size-procedure");
  if (gh_string_p (s) && gh_procedure_p (barsiz_proc))
    {
      String str  =ly_scm2string (s);
      SCM siz = gh_call1 (barsiz_proc, me->self_scm ());
      Real sz =  gh_scm2double (siz);
      if (sz < 0)
	return SCM_EOL;
      
      return compound_barline (me, str, sz).smobbed_copy ();
    }
  return SCM_EOL;
}


Molecule
Bar::compound_barline (Grob*me, String str, Real h)
{
  Real kern = gh_scm2double (me->get_grob_property ("kern"));
  Real thinkern = gh_scm2double (me->get_grob_property ("thin-kern"));
  Real hair = gh_scm2double (me->get_grob_property ("hair-thickness"));
  Real fatline = gh_scm2double (me->get_grob_property ("thick-thickness"));

  Real staffline = me->paper_l ()->get_var ("stafflinethickness");
  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Real staffspace = me->paper_l ()->get_var ("staffspace")
    * staff_space;

  kern *= staffline;
  thinkern *= staffline;
  hair *= staffline;
  fatline *= staffline;
  
  Molecule thin = simple_barline (me, hair, h);
  Molecule thick = simple_barline (me, fatline, h);
  Molecule colon;
  Molecule dot = Font_interface::get_default_font (me)->find_by_name ("dots-dot");
  Real dist = ( Staff_symbol_referencer::line_count (me) & 1 ? 1 :
		staff_space<2 ? 2 : .5 ) * staffspace;
  dot.translate_axis(dist/2,Y_AXIS);
  colon.add_molecule(dot);
  dot.translate_axis(-dist,Y_AXIS);
  colon.add_molecule(dot);

  Molecule m;
  
  if (str == "")
    {
      return Lookup::blank (Box (Interval (0, 0), Interval (-h/2, h/2)));
    }
  else if (str == "|")
    {
      return thin;
    }
  else if (str == "|." || (h == 0 && str == ":|"))
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);      
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
    }
  else if (str == ".|" || (h == 0 && str == "|:"))
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
    }
  else if (str == ":|")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
    }
  else if (str == "|:")
    {
      m.add_at_edge (X_AXIS, RIGHT, thick, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ":|:")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, LEFT, colon, kern);      
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);
      m.add_at_edge (X_AXIS, RIGHT, colon, kern);      
    }
  else if (str == ".|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, thinkern);
      m.add_at_edge (X_AXIS, RIGHT, thick, kern);      
    }
  else if (str == "||")
    {
      m.add_at_edge (X_AXIS, RIGHT, thin, 0);
      m.add_at_edge (X_AXIS, RIGHT, thin, thinkern);      
    }

  return m;
}


Molecule
Bar::simple_barline (Grob*,Real w, Real h) 
{
  return Lookup::filledbox (Box (Interval (0,w), Interval (-h/2, h/2)));
}

MAKE_SCHEME_CALLBACK (Bar,before_line_breaking ,1);

SCM
Bar::before_line_breaking (SCM smob)
{
  Grob*me=unsmob_grob (smob);
  Item * item = dynamic_cast<Item*> (me);
  
  SCM g = me->get_grob_property ("glyph");
  SCM orig = g;
  Direction bsd = item->break_status_dir ();
  if (gh_string_p (g) && bsd)
    {
      SCM proc = me->get_grob_property ("break-glyph-function");
      g = gh_call2 (proc, g, gh_int2scm (bsd));
    }

  
  if (!gh_string_p (g))
    {
      me->set_grob_property ("molecule-callback", SCM_EOL);
      me->set_extent_callback (SCM_EOL, X_AXIS);
      // leave y_extent for spanbar? 
    }

  if (! gh_equal_p (g, orig))
    me->set_grob_property ("glyph", g);

  return SCM_UNSPECIFIED;
}
  
void
Bar::set_interface (Grob*me)
{
  me->set_interface (ly_symbol2scm ("bar-line-interface"));
}

bool
Bar::has_interface (Grob*m)
{
  return m && m->has_interface (ly_symbol2scm ("bar-line-interface"));
}


MAKE_SCHEME_CALLBACK (Bar,get_staff_bar_size,1);
SCM
Bar::get_staff_bar_size (SCM smob) 
{
  Grob*me = unsmob_grob (smob);
  Real ss = Staff_symbol_referencer::staff_space (me);
  SCM size = me->get_grob_property ("bar-size");
  if (gh_number_p (size))
    return gh_double2scm (gh_scm2double (size)*ss);
  else if (Staff_symbol_referencer::staff_symbol_l (me))
    {
      /*
	If there is no staff-symbol, we get -1 from the next
	calculation. That's a nonsense value, which would collapse the
	barline so we return 0.0 in the next alternative.
      */
      return gh_double2scm ((Staff_symbol_referencer::line_count (me) -1) * ss);
    }
  else
    return gh_int2scm (0);
}
