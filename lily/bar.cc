/*
  bar.cc -- implement Bar

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/
#include <math.h>

#include "main.hh"
#include "dimensions.hh"
#include "dimension-cache.hh"
#include "bar.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "debug.hh"
#include "all-font-metrics.hh"

Bar::Bar (SCM s)
  : Item (s)
{
}


Real
Bar::get_bar_size () const
{
  // Never called!
  return 0;
}


SCM 
Bar::brew_molecule (SCM smob) 
{
  Score_element * self = unsmob_element (smob);
  Bar * fly = dynamic_cast<Bar*> (self);
  SCM s = self->get_elt_property ("glyph");
  if (gh_string_p (s))
    {
      String str  =ly_scm2string (s);
      return fly->compound_barline (str, fly->get_bar_size ()).create_scheme ();
    }
  return SCM_EOL;
}

MAKE_SCHEME_SCORE_ELEMENT_CALLBACK(Bar,brew_molecule);

Molecule
Bar::compound_barline (String str, Real h) const
{
  Real kern = gh_scm2double (get_elt_property ("kern"));
  Real thinkern = gh_scm2double (get_elt_property ("thin-kern"));
  Real hair = gh_scm2double (get_elt_property ("hair-thickness"));
  Real fatline = gh_scm2double (get_elt_property ("thick-thickness"));

  Real staffline = paper_l ()->get_var ("stafflinethickness");

  kern *= staffline;
  thinkern *= staffline;
  hair *= staffline;
  fatline *= staffline;
  
  Molecule thin = simple_barline (hair, h);
  Molecule thick = simple_barline (fatline, h);
  Molecule colon = lookup_l ()->afm_find ("dots-repeatcolon");  

  Molecule m;
  
  if (str == "")
    {
      return lookup_l ()->blank (Box (Interval(0, 0), Interval (-h/2, h/2)));
    }
  if (str == "scorepostbreak")
    {
      return simple_barline (paper_l ()->get_var ("barthick_score"), h);
    }
  else if (str == "|")
    {
      return thin;
    }
  else if (str == "|.")
    {
      m.add_at_edge (X_AXIS, LEFT, thick, 0);      
      m.add_at_edge (X_AXIS, LEFT, thin, kern);
    }
  else if (str == ".|")
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
Bar::simple_barline (Real w, Real h) const
{
  return lookup_l ()->filledbox (Box (Interval(0,w), Interval(-h/2, h/2)));
}


GLUE_SCORE_ELEMENT(Bar,before_line_breaking );
SCM
Bar::member_before_line_breaking  ()
{
  SCM g = get_elt_property ("glyph");
  SCM orig = g;
  Direction bsd = break_status_dir ();
  if (gh_string_p (g))
    {
      if (bsd)
	{
	  SCM breakdir = gh_int2scm (bsd);
	  g = scm_eval (gh_list (ly_symbol2scm ("break-barline"),
				 g,
				 breakdir,
				 SCM_UNDEFINED));
	}
    }
  else
    {
      g = SCM_UNDEFINED;
    }
  
  if (!gh_string_p (g))
    {
      set_elt_property ("molecule-callback", SCM_BOOL_T);
      set_extent_callback (0, X_AXIS);
      // leave y_extent for spanbar? 
    }
  else if (! gh_equal_p  (g, orig))
    set_elt_property ("glyph", g);


  return SCM_UNDEFINED;
}
  

