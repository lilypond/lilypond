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

void
Clef_item::do_pre_processing()
{
  translate_axis (y_position_i_ * staff_line_leading_f () / 2.0, Y_AXIS);
  SCM style_sym =get_elt_property (style_scm_sym);
  String style;
  if (style_sym != SCM_BOOL_F)
    style = ly_scm2string (SCM_CDR(style_sym));
  
  if (break_status_dir() != RIGHT && style != "fullSizeChanges")
    symbol_ += "_change";
  if (style == "transparent")
    {
      set_elt_property (transparent_scm_sym, SCM_BOOL_T);
      set_empty (true, X_AXIS);
    }
}

/*
  FIXME
*/
Clef_item::Clef_item()
{
  set_elt_property (breakable_scm_sym, SCM_BOOL_T);

  symbol_ = "treble";
  y_position_i_ = -2;
}

void
Clef_item::do_add_processing ()
{
  if (!break_status_dir ())	// broken stuff takes care of their own texts
    {
      Text_item *g =0;

      SCM octave_dir = remove_elt_property (octave_dir_scm_sym);
      if (octave_dir != SCM_BOOL_F)
	{
	  Direction d = Direction (gh_scm2int (SCM_CDR(octave_dir)));
	  g = new Text_item;
	  pscore_l_->typeset_element (g);
      
	  g->text_str_ = "8";
	  g->set_elt_property (style_scm_sym, gh_str02scm ("italic"));
	  g->set_parent (this, Y_AXIS);
	  g->set_parent (this, X_AXIS);	  

	  add_dependency (g);	// just to be sure.

	  Real r = do_height ()[d] - g->extent (Y_AXIS)[-d];
	  g->dim_cache_[Y_AXIS]->set_offset (r);

	  SCM my_vis = get_elt_property (visibility_lambda_scm_sym);
	  if (my_vis != SCM_BOOL_F)
	    g->set_elt_property (visibility_lambda_scm_sym, SCM_CDR (my_vis));
			
	}

    }
}

Molecule*
Clef_item::do_brew_molecule_p() const
{
  Molecule*output = new Molecule (lookup_l ()->clef (symbol_));
  return output;
}



