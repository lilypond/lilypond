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
#include "g-text-item.hh"
#include "p-score.hh"

void
Clef_item::do_pre_processing()
{
  dim_cache_[Y_AXIS].translate (y_position_i_ * staff_line_leading_f () / 2.0);
  if (break_status_dir() != RIGHT)
    {
      symbol_ += "_change";    
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
      G_text_item *g =0;

      SCM octave_dir = remove_elt_property (octave_dir_scm_sym);
      if (octave_dir != SCM_BOOL_F)
	{
	  Direction d = Direction (gh_int2scm (SCM_CDR(octave_dir)));
	  g = new G_text_item;
	  pscore_l_->typeset_element (g);
      
	  g->text_str_ = "8";
	  g->style_str_ = "italic";
	  g->dim_cache_[Y_AXIS].parent_l_ = &dim_cache_[Y_AXIS];
	  g->dim_cache_[X_AXIS].parent_l_ = &dim_cache_[X_AXIS];
	  add_dependency (g);	// just to be sure.

	  Real r = do_height ()[d] + g->extent (Y_AXIS)[-d];
	  g->dim_cache_[Y_AXIS].set_offset (r);
	  g->set_elt_property (visibility_lambda_scm_sym,
			       get_elt_property (visibility_lambda_scm_sym));
	}

    }
}

Molecule*
Clef_item::do_brew_molecule_p() const
{
  Molecule*output = new Molecule (lookup_l ()->clef (symbol_));
  return output;
}



