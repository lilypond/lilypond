/*
  vaticana-ligature-engraver.cc -- implement Vaticana_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (C) 2003 Juergen Reuter <reuter@ipd.uka.de>
 */

#include "gregorian-ligature-engraver.hh"
#include "gregorian-ligature.hh"
#include "vaticana-ligature.hh"
#include "item.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "paper-def.hh"

class Vaticana_ligature_engraver : public Gregorian_ligature_engraver
{

private:
  Real finish_primitive (Item *first_primitive,
			 Item *primitive,
			 int context_info,
			 String glyph_name,
			 int pitch_delta,
			 Real flexa_width,
			 Real join_thickness,
			 Real distance);

public:
  TRANSLATOR_DECLARATIONS(Vaticana_ligature_engraver);

protected:
  virtual Spanner *create_ligature_spanner ();
  virtual void transform_heads (Spanner *ligature,
				Array<Grob_info> primitives);
};

Vaticana_ligature_engraver::Vaticana_ligature_engraver ()
{
}

Spanner *
Vaticana_ligature_engraver::create_ligature_spanner ()
{
  return new Spanner (get_property ("VaticanaLigature"));
}

Real
Vaticana_ligature_engraver::finish_primitive (Item *first_primitive,
					      Item *primitive,
					      int context_info,
					      String glyph_name,
					      int pitch_delta,
					      Real flexa_width,
					      Real join_thickness,
					      Real distance)
{
  if (primitive)
    {
      // determine width of previous head and x-offset
      Real head_width;
      Real x_offset;
      bool is_stacked;
      is_stacked = context_info & PES_UPPER;
      if (context_info & FLEXA_LEFT)
	is_stacked = false;
      if (!String::compare (glyph_name, "vaticana_cephalicus") &&
	  !(context_info & PES_LOWER))
	is_stacked = true;
      if (context_info & AUCTUM)
	is_stacked = false;
      if (is_stacked)
	{
	  /*
	   * This head is stacked upon another one; hence, it does not
	   * contribute to the total width of the ligature, hence its
	   * width is assumed to be 0.0.  Moreover, it is shifted to
	   * the left by its width such that the right side of this
	   * and the other head are horizontally aligned.
	   */
	  head_width = 0.0;
	  x_offset = join_thickness -
	    Font_interface::get_default_font (primitive)->
	    find_by_name ("noteheads-" + glyph_name).extent (X_AXIS).length ();
	}
      else if (!String::compare (glyph_name, "porrectus") ||
	       !String::compare (glyph_name, ""))
	{
	  /*
	   * This head represents either half of a porrectus shape.
	   * Hence, it is assigned half the width of this shape.
	   */
	  head_width = 0.5 * flexa_width;
	  x_offset = 0.0;
	}
      else // retrieve width from corresponding font
	{
	  head_width =
	    Font_interface::get_default_font (primitive)->
	    find_by_name ("noteheads-" + glyph_name).extent (X_AXIS).length ();
	  x_offset = 0.0;
	}

      /*
       * Save the head's final shape and x-offset.
       */
      primitive->set_grob_property ("glyph-name",
				    scm_makfrom0str (glyph_name.to_str0 ()));
      primitive->set_grob_property ("x-offset",
				    gh_double2scm (x_offset));

      /*
       * If the head is the 2nd head of a pes or flexa (but not a
       * porrectus), mark this head to be joined with the left-side
       * neighbour head (i.e. the previous head) by a vertical beam.
       */
      if ((context_info & PES_UPPER) ||
	  ((context_info & FLEXA_RIGHT) &&
	   !(context_info & PES_LOWER)))
	{
	  primitive->set_grob_property ("join-left",
					gh_int2scm (pitch_delta));

	  /*
	   * Create a small overlap of adjacent heads so that the join
	   * can be drawn perfectly between them.
	   */
	  distance -= join_thickness;
	}
      else
	{
	  /*
	   * Make a small space between adjacent notes of a ligature
	   * that are not directly joined.
	   */
	  distance += 2 * join_thickness;
	}

      /*
       * Horizontally line-up this head to form a ligature.
       */
      get_set_column (primitive, first_primitive->get_column ());
      primitive->translate_axis (distance, X_AXIS);
      distance += head_width;
    }
  return distance;
}

void
Vaticana_ligature_engraver::transform_heads (Spanner *ligature,
					     Array<Grob_info> primitives)
{
  Real flexa_width;
  SCM flexa_width_scm = ligature->get_grob_property ("flexa-width");
  if (flexa_width_scm != SCM_EOL)
    {
      flexa_width = gh_scm2double (flexa_width_scm);
    }
  else
    {
      programming_error ("Vaticana_ligature_engraver:"
			 "flexa-width undefined; assuming 2.0 staff space");
      flexa_width =
	2.0 * Staff_symbol_referencer::staff_space (ligature);
    }

  Real join_thickness;
  SCM join_thickness_scm = ligature->get_grob_property ("thickness");
  if (join_thickness_scm != SCM_EOL)
    {
      join_thickness = gh_scm2double (join_thickness_scm);
    }
  else
    {
      programming_error ("Vaticana_ligature_engraver:"
			 "thickness undefined; assuming 1.4 linethickness");
      join_thickness = 1.4;
    }
  join_thickness *= ligature->get_paper ()->get_var ("linethickness");

  Item *first_primitive = 0;
  Item *prev_primitive = 0;
  int prev_context_info = 0;
  int prev_pitch = 0;
  int prev_pitch_delta = 0;
  String prev_glyph_name = "";
  Real prev_distance = 0.0;
  for (int i = 0; i < primitives.size(); i++) {
    Item *primitive = dynamic_cast<Item*> (primitives[i].grob_);
    Music *music_cause = primitives[i].music_cause ();
    int context_info = gh_scm2int (primitive->get_grob_property ("context-info"));
    int pitch = unsmob_pitch (music_cause->get_mus_property ("pitch"))->steps ();
    String glyph_name;
    if (!first_primitive)
      first_primitive = primitive;
    int prefix_set = gh_scm2int (primitive->get_grob_property ("prefix-set"));

    /*
     * Now determine which head to typeset (this is context sensitive
     * information, since it depends on neighbouring heads; therefore,
     * this decision must be made here in the engraver rather than in
     * the backend).
     */
    if (prefix_set & VIRGA)
      glyph_name = "vaticana_virga";
    else if (prefix_set & QUILISMA)
      glyph_name = "vaticana_quilisma";
    else if (prefix_set & ORISCUS)
      glyph_name = "solesmes_oriscus";
    else if (prefix_set & STROPHA)
      if (prefix_set & AUCTUM)
	glyph_name = "solesmes_stropha_aucta";
      else glyph_name = "solesmes_stropha";
    else if (prefix_set & SEMIVOCALIS)
      if (pitch > prev_pitch)
	glyph_name = "vaticana_epiphonus";
      else glyph_name = "vaticana_cephalicus";
    else if (prefix_set & INCLINATUM)
      if (prefix_set & AUCTUM)
	glyph_name = "solesmes_incl_auctum";
      else if (prefix_set & DEMINUTUM)
	glyph_name = "solesmes_incl_parvum";
      else
	glyph_name = "vaticana_inclinatum";
    else if (prefix_set & (CAVUM | LINEA))
      if ((prefix_set & CAVUM) && (prefix_set & LINEA))
	glyph_name = "vaticana_linea_punctum_cavum";
      else if (prefix_set & CAVUM)
	glyph_name = "vaticana_punctum_cavum";
      else
	glyph_name = "vaticana_linea_punctum";
    else if (prefix_set & AUCTUM)
      if (prefix_set & ASCENDENS)
	glyph_name = "solesmes_auct_asc";
      else
	glyph_name = "solesmes_auct_desc";
    else if (prefix_set & DEMINUTUM)
      glyph_name = "vaticana_plica";
    else if ((prefix_set & PES_OR_FLEXA) &&
	     (context_info & PES_LOWER) &&
	     (context_info & FLEXA_RIGHT))
      glyph_name = ""; // second head of porrectus
    else if (context_info & PES_UPPER)
      if (pitch - prev_pitch > 1)
	glyph_name = "vaticana_upes";
      else
	glyph_name = "vaticana_vupes";
    else
      glyph_name = "vaticana_punctum";

    /*
     * May need updating previous head, depending on the current head.
     */
    if (prefix_set & PES_OR_FLEXA)
      if ((context_info & PES_LOWER) &&
	  (context_info & FLEXA_RIGHT)) // porrectus
	{
	  prev_glyph_name = "porrectus";
	  prev_primitive->set_grob_property ("porrectus-height",
					     gh_int2scm (pitch - prev_pitch));
	  prev_primitive->set_grob_property ("porrectus-width",
					     gh_double2scm (flexa_width));
	  bool add_stem =
	    !(prev_context_info & PES_UPPER) &&
	    !(prev_context_info & FLEXA_RIGHT);
	  prev_primitive->set_grob_property ("add-stem",
					     gh_bool2scm (add_stem));
	}
      else if (context_info & PES_UPPER)
	{
	  if (!String::compare (prev_glyph_name, "vaticana_punctum"))
	    prev_glyph_name = "vaticana_lpes";
	}
      else // flexa
	{
	  if (!String::compare (prev_glyph_name, "vaticana_punctum"))
	    prev_glyph_name = "vaticana_rvirga";
	}

    /*
     * In the backend, porrectus and joins need to know about
     * thickness.  Hence, for simplicity, let's distribute the
     * ligature grob's value for thickness to each ligature head (even
     * if not all of them need to know).
     */
    primitive->set_grob_property ("thickness", gh_double2scm (join_thickness));

    /*
     * The head of the current iteration still may change during the
     * next iteration due to the context sensitiveness of the
     * transformation.  However, the head of the previous iteration is
     * now fully attributed and can be prepared for the backend.
     */

    /*
     * Finish head of previous iteration for backend.
     */
    prev_distance =
      finish_primitive (first_primitive, prev_primitive,
			prev_context_info, prev_glyph_name, prev_pitch_delta,
			flexa_width, join_thickness, prev_distance);

    prev_primitive = primitive;
    prev_context_info = context_info;
    prev_pitch_delta = pitch - prev_pitch;
    prev_pitch = pitch;
    prev_glyph_name = glyph_name;
  }

  /*
   * Finish head of last iteration for backend.
   */
  finish_primitive (first_primitive, prev_primitive,
		    prev_context_info, prev_glyph_name, prev_pitch_delta,
		    flexa_width, join_thickness, prev_distance);
}


ENTER_DESCRIPTION (Vaticana_ligature_engraver,
/* descr */       "Handles ligatures by glueing special ligature heads together.",
/* creats*/       "VaticanaLigature",
/* accepts */     "ligature-event abort-event",
/* acks  */      "ligature-head-interface note-head-interface rest-interface",
/* reads */       "",
/* write */       "");
