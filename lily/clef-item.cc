/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <ctype.h>
#include "clef-item.hh"
#include "string.hh"
#include "molecule.hh"
#include "paper-def.hh"
#include "lookup.hh"
#include "text-item.hh"
#include "paper-score.hh"
#include "dimension-cache.hh"
#include "side-position-interface.hh"

void
Clef_item::do_pre_processing()
{
  SCM style_sym =get_elt_property ("style");
  String style;
  if (style_sym != SCM_UNDEFINED)
    style = ly_scm2string (style_sym);
  
  if (break_status_dir() != RIGHT && style != "fullSizeChanges")
    symbol_ += "_change";
  if (style == "transparent")
    {
      set_elt_property ("transparent", SCM_BOOL_T);
      set_empty (X_AXIS);
    }
}

/*
  FIXME
*/
Clef_item::Clef_item()
{
  set_elt_property ("breakable", SCM_BOOL_T);

  symbol_ = "treble";
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
	  
	  pscore_l_->typeset_element (g);
      
	  g->text_str_ = "8";
	  g->set_elt_property ("style", gh_str02scm ("italic"));
	  g->set_parent (this, Y_AXIS);
	  g->set_parent (this, X_AXIS);	  
	  g->set_elt_property ("direction", octave_dir);
	  
	  add_dependency (g);	// just to be sure.
	  SCM my_vis = get_elt_property ("visibility-lambda");
	  if (my_vis != SCM_UNDEFINED)
	    g->set_elt_property ("visibility-lambda", my_vis);
			
	}

    }
}

Molecule*
Clef_item::do_brew_molecule_p() const
{
  Molecule*output = new Molecule (lookup_l ()->afm_find (String ("clefs-" + symbol_)));

  return output;
}




