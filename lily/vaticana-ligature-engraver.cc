/*
  vaticana-ligature-engraver.cc -- implement Vaticana_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c)  2003 Juergen Reuter <reuter@ipd.uka.de>
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
#include "paper-column.hh"

/*
 * This class implements the notation specific aspects of Vaticana
 * style ligatures for Gregorian chant notation.
 */
class Vaticana_ligature_engraver : public Gregorian_ligature_engraver
{

private:
  bool is_stacked_head (int prefix_set,
			int context_info);
  Real align_heads (Array<Grob_info> primitives,
		    Real flexa_width,
		    Real join_thickness);

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

bool
Vaticana_ligature_engraver::is_stacked_head (int prefix_set,
					     int context_info)
{
      bool is_stacked_b;

      // upper head of pes is stacked upon lower head of pes ...
      is_stacked_b = context_info & PES_UPPER;

      // ... unless this note starts a flexa
      if (context_info & FLEXA_LEFT)
	is_stacked_b = false;

      // ... or another pes
      if (context_info & PES_LOWER)
	is_stacked_b = false;

      // ... or the previous note is a semivocalis or inclinatum
      if (context_info & AFTER_DEMINUTUM)
	is_stacked_b = false;

      // auctum head is never stacked upon preceding note
      if (prefix_set & AUCTUM)
	is_stacked_b = false;

      // virga is never stacked upon preceding note
      if (prefix_set & VIRGA)
	is_stacked_b = false;

      // oriscus is never stacked upon preceding note
      if (prefix_set & ORISCUS)
	is_stacked_b = false;

      if ((prefix_set & DEMINUTUM) &&
	  !(prefix_set & INCLINATUM) &&
	  (context_info & FLEXA_RIGHT))
	is_stacked_b = true; // semivocalis head of deminutus form

      return is_stacked_b;
}

inline int get_context_info (Item *primitive)
{
  SCM context_info_scm = primitive->get_grob_property ("context-info");
  if (context_info_scm != SCM_EOL)
    {
      return gh_scm2int (context_info_scm);
    }
  else
    {
      primitive->programming_error ("Vaticana_ligature:"
				    "context-info undefined -> "
				    "ignoring grob");
      return -1;
    }
}

Real
Vaticana_ligature_engraver::align_heads (Array<Grob_info> primitives,
					 Real flexa_width,
					 Real join_thickness)
{
  if (!primitives.size ())
    {
      programming_error ("Vaticana_ligature: "
			 "empty ligature [ignored]");
      return 0.0;
    }

  Item *first_primitive = dynamic_cast<Item*> (primitives[0].grob_);
  Real ligature_width = 0.0;

  /*
   * Amount of extra space two put between some particular
   * configurations of adjacent heads.
   *
   * TODO: make this a property of primtive grobs.
   */
  Real extra_space = 2.0 * join_thickness;

  Item *prev_primitive, *primitive, *next_primitive;
  int prev_context_info, context_info, next_context_info;

  primitive = 0;
  context_info = 0;

  next_primitive = first_primitive;
  if ((next_context_info = get_context_info (next_primitive)) < 0)
    {
      return 0.0;
    }

  for (int i = 0; i < primitives.size (); i++)
    {
      prev_primitive = primitive;
      prev_context_info = context_info;
      context_info = next_context_info;
      primitive = next_primitive;

      if (i+1 < primitives.size ())
	{
	  next_primitive = dynamic_cast<Item*> (primitives[i+1].grob_);
	  if ((next_context_info = get_context_info (next_primitive)) < 0)
	    {
	      break;
	    }
	}
      else
	{
	  next_primitive = 0;
	  next_context_info = 0;
	}

      /*
       * Get glyph_name, delta_pitch and context_info for this head.
       */

      SCM glyph_name_scm = primitive->get_grob_property ("glyph-name");
      if (glyph_name_scm == SCM_EOL)
	{
	  primitive->programming_error ("Vaticana_ligature:"
					"undefined glyph-name -> "
					"ignoring grob");
	  continue;
	}
      String glyph_name = ly_scm2string (glyph_name_scm);

      int delta_pitch;
      SCM delta_pitch_scm = primitive->get_grob_property ("delta-pitch");
      if (delta_pitch_scm != SCM_EOL)
	{
	  delta_pitch = gh_scm2int (delta_pitch_scm);
	}
      else
	{
	  primitive->programming_error ("Vaticana_ligature:"
					"delta-pitch undefined -> "
					"ignoring grob");
	  continue;
	}

      /*
       * Now determine width and x-offset of head.
       */

      Real head_width;
      Real x_offset;

      if (context_info & STACKED_HEAD)
	{
	  /*
	   * This head is stacked upon the previous one; hence, it
	   * does not contribute to the total width of the ligature,
	   * and its width is assumed to be 0.0.  Moreover, it is
	   * shifted to the left by its width such that the right side
	   * of this and the other head are horizontally aligned.
	   */
	  head_width = 0.0;
	  x_offset = join_thickness -
	    Font_interface::get_default_font (primitive)->
	    find_by_name ("noteheads-" + glyph_name).extent (X_AXIS).length ();
	}
      else if (!String::compare (glyph_name, "flexa") ||
	       !String::compare (glyph_name, ""))
	{
	  /*
	   * This head represents either half of a flexa shape.
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
       * Save the head's final x-offset.
       */
      primitive->set_grob_property ("x-offset",
				    gh_double2scm (x_offset));

      /*
       * If the head is the 2nd head of a pes or flexa (but not a
       * flexa shape), mark this head to be joined with the left-side
       * neighbour head (i.e. the previous head) by a vertical beam.
       */
      if ((context_info & PES_UPPER) ||
	  ((context_info & FLEXA_RIGHT) &&
	   !(context_info & PES_LOWER)))
	{
	  primitive->set_grob_property ("join-left", gh_bool2scm(true));

	  /*
	   * Create a small overlap of adjacent heads so that the join
	   * can be drawn perfectly between them.
	   */
	  ligature_width -= join_thickness;
	}
      else if (!String::compare (glyph_name, ""))
	{
	  /*
	   * This is the 2nd (virtual) head of flexa shape.  Join it
	   * tightly with 1st head, i.e. do *not* add additional
	   * space, such that next head will not be off from the flexa
	   * shape.
	   */
	}

      /* Sometimes, extra space is needed, e.g. to avoid clashing with
	 the appendix of an adjacent notehead or with an adjacent
	 notehead itself if it has the same pitch. */

      if (context_info & AFTER_VIRGA)
	{
	  /*
	   * After a virga, make a an additional small space such that
	   * the appendix on the right side of the head does not touch
	   * the following head.
	   */
	  ligature_width += extra_space;
	}
      else if ((context_info & FLEXA_LEFT) &&
	       !(prev_context_info & PES_LOWER))
	{
	  /*
	   * Before a flexa (but not within a torculus), make a an
	   * additional small space such that the appendix on the left
	   * side of the flexa does not touch the this head.
	   */
	  ligature_width += extra_space;
	}
      else if (delta_pitch == 0)
	{
	  /*
	   * If there are two adjacent noteheads with the same pitch,
	   * add additional small space between them, such that they
	   * do not touch each other.
	   */
	  ligature_width += extra_space;
	}

      /*
       * Horizontally line-up this head to form a ligature.
       */
      get_set_column (primitive, first_primitive->get_column ());
      primitive->translate_axis (ligature_width, X_AXIS);
      ligature_width += head_width;

    }

  /*
   * Add extra horizontal padding space after ligature, such that
   * neighbouring ligatures do not touch each other.
   */
  ligature_width += extra_space;

  return ligature_width;
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
      ligature->programming_error ("Vaticana_ligature_engraver:"
				   "flexa-width undefined; "
				   "assuming 2.0 staff space");
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
      ligature->programming_error ("Vaticana_ligature_engraver:"
				   "thickness undefined; "
				   "assuming 1.4 linethickness");
      join_thickness = 1.4;
    }
  join_thickness *= ligature->get_paper ()->get_realvar (ly_symbol2scm ("linethickness"));

  Item *prev_primitive = 0;
  int prev_prefix_set = 0;
  int prev_context_info = 0;
  int prev_pitch = 0;
  String prev_glyph_name = "";
  for (int i = 0; i < primitives.size(); i++) {
    Item *primitive = dynamic_cast<Item*> (primitives[i].grob_);
    Music *music_cause = primitives[i].music_cause ();

    /* compute interval between previous and current primitive */
    int pitch =
      unsmob_pitch (music_cause->get_mus_property ("pitch"))->steps ();
    int delta_pitch;
    if (i == 0)
      {
	delta_pitch = 0;
      }
    else
      {
	delta_pitch = pitch - prev_pitch;
      }

    /* retrieve & complete prefix_set and context_info */
    int prefix_set =
      gh_scm2int (primitive->get_grob_property ("prefix-set"));
    int context_info =
      gh_scm2int (primitive->get_grob_property ("context-info"));
    if (is_stacked_head (prefix_set, context_info))
      {
	context_info |= STACKED_HEAD;
	primitive->set_grob_property ("context-info",
				      gh_int2scm (context_info));
      }

    /*
     * Now determine which head to typeset (this is context sensitive
     * information, since it depends on neighbouring heads; therefore,
     * this decision must be made here in the engraver rather than in
     * the backend).
     */
    String glyph_name;
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
    else if (prefix_set & INCLINATUM)
      if (prefix_set & AUCTUM)
	glyph_name = "solesmes_incl_auctum";
      else if (prefix_set & DEMINUTUM)
	glyph_name = "solesmes_incl_parvum";
      else
	glyph_name = "vaticana_inclinatum";
    else if (prefix_set & DEMINUTUM)
      if (i == 0)
	{
	  // initio debilis
	  glyph_name = "vaticana_reverse_plica";
	}
      else if (delta_pitch > 0)
	{
	  // epiphonus
	  if (!(prev_context_info & FLEXA_RIGHT))
	    {
	      prev_glyph_name = "vaticana_epiphonus";
	    }
	  glyph_name = "vaticana_plica";
	}
      else // (delta_pitch <= 0)
	{
	  // cephalicus
	  if (!(prev_context_info & FLEXA_RIGHT))
	    {
	      if (i > 1)
		{
		  prev_glyph_name = "vaticana_inner_cephalicus";
		}
	      else
		{
		  prev_glyph_name = "vaticana_cephalicus";
		}
	    }
	  glyph_name = "vaticana_reverse_plica";
	}
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
    else if ((prefix_set & PES_OR_FLEXA) &&
	     (context_info & PES_LOWER) &&
	     (context_info & FLEXA_RIGHT))
      glyph_name = ""; // second head of flexa shape
    else if ((context_info & STACKED_HEAD) &&
	     (context_info & PES_UPPER))
      if (delta_pitch > 1)
	glyph_name = "vaticana_upes";
      else
	glyph_name = "vaticana_vupes";
    else if ((context_info & FLEXA_LEFT) &&
	     !(prefix_set && PES_OR_FLEXA))
      glyph_name = "vaticana_rvirga";
    else
      glyph_name = "vaticana_punctum";

    /*
     * If the head for the current primitive represents the right head
     * of a flexa or the upper head of a pes, then this may affect the
     * shape of the previous head.
     */
    if (prefix_set & PES_OR_FLEXA)
      {
	if ((context_info & FLEXA_RIGHT) && (context_info & PES_LOWER))
	  {
	    // join the two flexa heads into a single curved flexa shape
	    prev_glyph_name = "flexa";
	    prev_primitive->set_grob_property ("flexa-height",
					       gh_int2scm (delta_pitch));
	    prev_primitive->set_grob_property ("flexa-width",
					       gh_double2scm (flexa_width));
	    bool add_stem = !(prev_prefix_set && PES_OR_FLEXA);
	    prev_primitive->set_grob_property ("add-stem",
					       gh_bool2scm (add_stem));
	  }
	if ((context_info & PES_UPPER) && (context_info & STACKED_HEAD))
	  {
	    if (!String::compare (prev_glyph_name, "vaticana_punctum"))
	      prev_glyph_name = "vaticana_lpes";
	  }
      }

    if (prev_primitive)
      prev_primitive->set_grob_property ("glyph-name",
					 scm_makfrom0str (prev_glyph_name.to_str0 ()));

    primitive->set_grob_property ("delta-pitch",
				  gh_int2scm (delta_pitch));

    /*
     * In the backend, flexa shapes and joins need to know about
     * thickness.  Hence, for simplicity, let's distribute the
     * ligature grob's value for thickness to each ligature head (even
     * if not all of them need to know).
     */
    primitive->set_grob_property ("thickness", gh_double2scm (join_thickness));

    prev_primitive = primitive;
    prev_prefix_set = prefix_set;
    prev_context_info = context_info;
    prev_pitch = pitch;
    prev_glyph_name = glyph_name;
  }

  prev_primitive->set_grob_property ("glyph-name",
				     scm_makfrom0str (prev_glyph_name.to_str0 ()));

#if 0
  Real ligature_width =
#endif

  align_heads (primitives, flexa_width, join_thickness);

#if 0 // experimental code to collapse spacing after ligature
  /* TODO: set to max(old/new spacing-increment), since other
     voices/staves also may want to set this property. */
  Item *first_primitive = dynamic_cast<Item*> (primitives[0].grob_);
  Paper_column *paper_column = first_primitive->get_column();
  paper_column->warning (_f ("Vaticana_ligature_engraver: "
			     "setting `spacing-increment = %f': ptr=%ul",
			     ligature_width, paper_column));
  paper_column->
    set_grob_property("forced-spacing", gh_double2scm (ligature_width));
#endif
}


ENTER_DESCRIPTION (Vaticana_ligature_engraver,
/* descr */       "Handles ligatures by glueing special ligature heads together.",
/* creats*/       "VaticanaLigature",
/* accepts */     "ligature-event abort-event",
/* acks  */      "note-head-interface rest-interface",
/* reads */       "",
/* write */       "");
