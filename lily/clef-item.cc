/*
  clef-item.cc -- implement Clef_item

  source file of the GNU LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
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
  dim_cache_[Y_AXIS].translate (paper()->internote_f () * y_position_i_);
  change_b_ = (break_status_dir() != RIGHT);
}

/*
  FIXME
*/
Clef_item::Clef_item()
{
  breakable_b_ =true;
  default_b_ = false;
  change_b_ = true;
  octave_dir_ = CENTER;
  symbol_ = "treble";
  y_position_i_ = -2;
}

void
Clef_item::do_add_processing ()
{
  if (!break_status_dir_)	// broken stuff takes care of their own texts
    {
      SCM defvis = gh_eval_str ("(lambda (d) (if (= d 1) '(#f . #f) '(#t . #t)))");
      G_text_item *g =0;
      if (octave_dir_)
	{
	  g = new G_text_item;
	  pscore_l_->typeset_element (g);
      
	  g->text_str_ = "8";
	  g->style_str_ = "italic";
	  g->dim_cache_[Y_AXIS].parent_l_ = &dim_cache_[Y_AXIS];
	  g->dim_cache_[X_AXIS].parent_l_ = &dim_cache_[X_AXIS];
	  add_dependency (g);	// just to be sure.

	  Real r = do_height ()[octave_dir_] + g->extent (Y_AXIS)[-octave_dir_];
	  g->dim_cache_[Y_AXIS].set_offset (r);
	}
      if (default_b_)
	{
	  visibility_lambda_ = defvis;
	  if (g)
	    g->visibility_lambda_ = defvis;
	}
    }


}





Molecule*
Clef_item::do_brew_molecule_p() const
{
  String t = symbol_;
  if  (change_b_)
    t += "_change";

  Molecule*output = new Molecule (lookup_l ()->clef (t));
  return output;
}



