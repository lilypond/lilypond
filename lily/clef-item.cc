/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "text-item.hh"
#include "paper-score.hh"
#include "dimension-cache.hh"
#include "side-position-interface.hh"
#include "warn.hh"
#include "line-of-score.hh"

void
Clef_item::before_line_breaking ()
{
  SCM style_sym =get_elt_property ("style");
  String style;
  if (gh_string_p (style_sym))
    style = ly_scm2string (style_sym);

  SCM glyph = get_elt_property ("glyph");
  if (gh_string_p (glyph))
    {
      String s = ly_scm2string (glyph);
	
      if (break_status_dir() != RIGHT && style != "fullSizeChanges")
	{
	  s += "_change";
	}
      s = "clefs-" +  s;
      set_elt_property ("glyph", ly_str02scm (s.ch_C()));
    }
  else
    {
      set_elt_property ("transparent", SCM_BOOL_T);
    }
  
  if (style == "transparent")	// UGH. JUNKME
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_extent_callback (0, X_AXIS);
    }
}


void
Clef_item::do_add_processing ()
{
  if (!break_status_dir ())	// broken stuff takes care of their own texts
    {
      Text_item *g =0;

      SCM octave_dir = remove_elt_property ("octave-dir");
      if (isdir_b (octave_dir))
	{
	  g = new Text_item;
	  Side_position_interface spi (g);
	  spi.set_axis (Y_AXIS);
	  
	  pscore_l_->line_l_->typeset_element (g);
      
	  spi.add_support (this);
	  g->set_elt_property ("text", ly_str02scm ( "8"));
	  g->set_elt_property ("style", gh_str02scm ("italic"));
	  g->set_parent (this, Y_AXIS);
	  g->set_parent (this, X_AXIS);
	  
	  g->set_elt_property ("self-alignment-X", gh_int2scm (0));
	  g->add_offset_callback (Side_position_interface::aligned_on_self, X_AXIS);
	  g->add_offset_callback (Side_position_interface::centered_on_parent, X_AXIS);

	  g->set_elt_property ("direction", octave_dir);
	  
	  add_dependency (g);	// just to be sure.
	  SCM my_vis = get_elt_property ("visibility-lambda");
	  if (gh_procedure_p (my_vis))
	    g->set_elt_property ("visibility-lambda", my_vis);
			
	}

    }
}


