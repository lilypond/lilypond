/*
  vaticana-ligature-engraver.cc -- implement Vaticana_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2005 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "gregorian-ligature-engraver.hh"
#include "gregorian-ligature.hh"
#include "vaticana-ligature.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "font-interface.hh"
#include "warn.hh"
#include "output-def.hh"
#include "paper-column.hh"

#include "translator.icc"

/*
 * This class implements the notation specific aspects of Vaticana
 * style ligatures for Gregorian chant notation.
 */

class Vaticana_ligature_engraver : public Gregorian_ligature_engraver
{

private:
  static bool
  need_extra_horizontal_space (int prev_prefix_set, int prefix_set,
			       int context_info, int delta_pitch);
  bool is_stacked_head (int prefix_set,
			int context_info);
  Real align_heads (Array<Grob_info> primitives,
		    Real flexa_width,
		    Real thickness);

public:
  TRANSLATOR_DECLARATIONS (Vaticana_ligature_engraver);

protected:
  virtual Spanner *create_ligature_spanner ();
  virtual void transform_heads (Spanner *ligature,
				Array<Grob_info> primitives);
};

Vaticana_ligature_engraver::Vaticana_ligature_engraver ()
{
  brew_ligature_primitive_proc = 
    Vaticana_ligature::brew_ligature_primitive_proc;
}

Spanner *
Vaticana_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("VaticanaLigature", SCM_EOL);
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

  if ((prefix_set & DEMINUTUM)
      && ! (prefix_set & INCLINATUM)
      && (context_info & FLEXA_RIGHT))
    is_stacked_b = true; // semivocalis head of deminutus form

  return is_stacked_b;
}

/*
 * When aligning the heads, sometimes extra space is needed, e.g. to
 * avoid clashing with the appendix of an adjacent notehead or with an
 * adjacent notehead itself if it has the same pitch.  Extra space is
 * added at most once between to heads.
 */
bool
Vaticana_ligature_engraver::need_extra_horizontal_space (int prev_prefix_set, int prefix_set,
							 int context_info, int delta_pitch)
{
  if (prev_prefix_set & VIRGA)
    /*
     * After a virga, make a an additional small space such that the
     * appendix on the right side of the head does not touch the
     * following head.
     */
    return true;

  if ((prefix_set & INCLINATUM)
      && ! (prev_prefix_set & INCLINATUM))
    /*
     * Always start a series of inclinatum heads with an extra space.
     */
    return true;

  if ((context_info & FLEXA_LEFT) && ! (context_info & PES_UPPER))
    /*
     * Before a flexa (but not within a torculus), make a an
     * additional small space such that the appendix on the left side
     * of the flexa does not touch the this head.
     */
    return true;

  if (delta_pitch == 0)
    /*
     * If there are two adjacent noteheads with the same pitch, add
     * additional small space between them, such that they do not
     * touch each other.
     */
    return true;

  return false;
}

Real
Vaticana_ligature_engraver::align_heads (Array<Grob_info> primitives,
					 Real flexa_width,
					 Real thickness)
{
  if (!primitives.size ())
    {
      programming_error ("Vaticana_ligature: "
			 "empty ligature [ignored]");
      return 0.0;
    }

  /*
   * The paper column where we put the whole ligature into.
   */
  Paper_column *column
    = dynamic_cast<Item *> (primitives[0].grob ())->get_column ();

  Real join_thickness
    = thickness * column->layout ()->get_dimension (ly_symbol2scm ("linethickness"));

  /*
   * Amount of extra space two put between some particular
   * configurations of adjacent heads.
   *
   * TODO: make this a property of primtive grobs.
   */
  Real extra_space = 4.0 * join_thickness;

  /*
   * Keep track of the total width of the ligature.
   */
  Real ligature_width = 0.0;

  Item *prev_primitive = 0;
  int prev_prefix_set = 0;
  for (int i = 0; i < primitives.size (); i++)
    {
      Item *primitive = dynamic_cast<Item *> (primitives[i].grob ());
      int prefix_set
	= scm_to_int (primitive->get_property ("prefix-set"));
      int context_info
	= scm_to_int (primitive->get_property ("context-info"));

      /*
       * Get glyph_name, delta_pitch and context_info for this head.
       */

      SCM glyph_name_scm = primitive->get_property ("glyph-name");
      if (glyph_name_scm == SCM_EOL)
	{
	  primitive->programming_error ("Vaticana_ligature:"
					"undefined glyph-name -> "
					"ignoring grob");
	  continue;
	}
      String glyph_name = ly_scm2string (glyph_name_scm);

      int delta_pitch = 0;
      if (prev_primitive) /* urgh, need prev_primitive only here */
	{
	  SCM delta_pitch_scm = prev_primitive->get_property ("delta-pitch");
	  if (delta_pitch_scm != SCM_EOL)
	    delta_pitch = scm_to_int (delta_pitch_scm);
	  else
	    {
	      primitive->programming_error ("Vaticana_ligature:"
					    "delta-pitch undefined -> "
					    "ignoring grob");
	      continue;
	    }
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
	  x_offset = join_thickness
	    - Font_interface::get_default_font (primitive)->
	    find_by_name ("noteheads.s" + glyph_name).extent (X_AXIS).length ();
	}
      else if (!String::compare (glyph_name, "flexa")
	       || !String::compare (glyph_name, ""))
	{
	  /*
	   * This head represents either half of a flexa shape.
	   * Hence, it is assigned half the width of this shape.
	   */
	  head_width = 0.5 * flexa_width;
	  x_offset = 0.0;
	}
      else
	{
	  /*
	   * This is a regular head, placed right to the previous one.
	   * Retrieve its width from corresponding font.
	   */
	  head_width
	    = Font_interface::get_default_font (primitive)->
	    find_by_name ("noteheads.s" + glyph_name).extent (X_AXIS).length ();
	  x_offset = 0.0;
	}

      /*
       * Save the head's final x-offset.
       */
      primitive->set_property ("x-offset",
			       scm_from_double (x_offset));

      /*
       * If the head is the 2nd head of a pes or flexa (but not a
       * flexa shape), mark this head to be joined with the left-side
       * neighbour head (i.e. the previous head) by a vertical beam.
       */
      if ((context_info & PES_UPPER)
	  || ((context_info & FLEXA_RIGHT)
	      && ! (context_info & PES_LOWER)))
	{
	  if (!prev_primitive)
	    {
	      primitive->programming_error ("vaticana ligature: add-join: "
					    "missing previous primitive");
	    }
	  else
	    {
	      prev_primitive->set_property ("add-join",
					    ly_bool2scm (true));

	      /*
	       * Create a small overlap of adjacent heads so that the join
	       * can be drawn perfectly between them.
	       */
	      ligature_width -= join_thickness;
	    }
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

      if (need_extra_horizontal_space (prev_prefix_set, prefix_set,
				       context_info, delta_pitch))
	ligature_width += extra_space;

      /*
       * Horizontally line-up this head to form a ligature.
       */
      get_set_column (primitive, column);
      primitive->translate_axis (ligature_width, X_AXIS);
      ligature_width += head_width;

      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
    }

  /*
   * Add extra horizontal padding space after ligature, such that
   * neighbouring ligatures do not touch each other.
   */
  ligature_width += extra_space;

  return ligature_width;
}

/*
 * Depending on the typographical features of a particular ligature
 * style, some prefixes may be ignored.  In particular, if a curved
 * flexa shape is produced, any prefixes to either of the two
 * contributing heads that would select a head other than punctum, is
 * by definition ignored.
 *
 * This function prints a warning, if the given primitive is prefixed
 * such that a head other than punctum would be chosen, if this
 * primitive were engraved as a stand-alone head.
 */
void
check_for_prefix_loss (Item *primitive)
{
  int prefix_set
    = scm_to_int (primitive->get_property ("prefix-set"));
  if (prefix_set & ~PES_OR_FLEXA)
    {
      String prefs = Gregorian_ligature::prefixes_to_str (primitive);
      primitive->warning (_f ("ignored prefix (es) `%s' of this head according "
			      "to restrictions of the selected ligature style",
			      prefs.to_str0 ()));
    }
}

void
Vaticana_ligature_engraver::transform_heads (Spanner *ligature,
					     Array<Grob_info> primitives)
{
  Real flexa_width = robust_scm2double (ligature->get_property ("flexa-width"), 2);

  Real thickness = robust_scm2double (ligature->get_property ("thickness"), 1);

  Item *prev_primitive = 0;
  int prev_prefix_set = 0;
  int prev_context_info = 0;
  int prev_delta_pitch = 0;
  String prev_glyph_name = "";
  for (int i = 0; i < primitives.size (); i++)
    {
      Item *primitive = dynamic_cast<Item *> (primitives[i].grob ());

      int delta_pitch;
      SCM delta_pitch_scm = primitive->get_property ("delta-pitch");
      if (delta_pitch_scm != SCM_EOL)
	delta_pitch = scm_to_int (delta_pitch_scm);
      else
	{
	  primitive->programming_error ("Vaticana_ligature:"
					"delta-pitch undefined -> "
					"ignoring grob");
	  continue;
	}

      /* retrieve & complete prefix_set and context_info */
      int prefix_set
	= scm_to_int (primitive->get_property ("prefix-set"));
      int context_info
	= scm_to_int (primitive->get_property ("context-info"));
      if (is_stacked_head (prefix_set, context_info))
	{
	  context_info |= STACKED_HEAD;
	  primitive->set_property ("context-info",
				   scm_from_int (context_info));
	}

      /*
       * Now determine which head to typeset (this is context sensitive
       * information, since it depends on neighbouring heads; therefore,
       * this decision must be made here in the engraver rather than in
       * the backend).
       */
      String glyph_name;
      if (prefix_set & VIRGA)
	{
	  glyph_name = "vaticana.punctum";
	  primitive->set_property ("add-stem", ly_bool2scm (true));
	}
      else if (prefix_set & QUILISMA)
	glyph_name = "vaticana.quilisma";
      else if (prefix_set & ORISCUS)
	glyph_name = "solesmes.oriscus";
      else if (prefix_set & STROPHA)
	if (prefix_set & AUCTUM)
	  glyph_name = "solesmes.stropha.aucta";
	else glyph_name = "solesmes.stropha";
      else if (prefix_set & INCLINATUM)
	if (prefix_set & AUCTUM)
	  glyph_name = "solesmes.incl.auctum";
	else if (prefix_set & DEMINUTUM)
	  glyph_name = "solesmes.incl.parvum";
	else
	  glyph_name = "vaticana.inclinatum";
      else if (prefix_set & DEMINUTUM)
	if (i == 0)
	  {
	    // initio debilis
	    glyph_name = "vaticana.reverse.plica";
	  }
	else if (prev_delta_pitch > 0)
	  {
	    // epiphonus
	    if (! (prev_context_info & FLEXA_RIGHT))
	      /* correct head of previous primitive */
	      if (prev_delta_pitch > 1)
		prev_glyph_name = "vaticana.epiphonus";
	      else
		prev_glyph_name = "vaticana.vepiphonus";
	    if (prev_delta_pitch > 1)
	      glyph_name = "vaticana.plica";
	    else
	      glyph_name = "vaticana.vplica";
	  }
	else if (prev_delta_pitch < 0)
	  {
	    // cephalicus
	    if (! (prev_context_info & FLEXA_RIGHT))
	      /* correct head of previous primitive */
	      {
		if (i > 1)
		  {
		    /* cephalicus head with fixed size cauda */
		    prev_glyph_name = "vaticana.inner.cephalicus";
		  }
		else
		  {
		    /* cephalicus head without cauda */
		    prev_glyph_name = "vaticana.cephalicus";
		  }

		/*
		 * Flexa has no variable size cauda if its left head is
		 * stacked on the right head.  This is true for
		 * cephalicus.  Hence, remove the cauda.
		 *
		 * Urgh: for the current implementation, this rule only
		 * applies for cephalicus; but it is a fundamental rule.
		 * Therefore, the following line of code should be
		 * placed somewhere else.
		 */
		prev_primitive->set_property ("add-cauda",
					      ly_bool2scm (false));
	      }
	    if (prev_delta_pitch < - 1)
	      glyph_name = "vaticana.reverse.plica";
	    else
	      glyph_name = "vaticana.reverse.vplica";
	  }
	else // (prev_delta_pitch == 0)
	  {
	    primitive->programming_error ("Vaticana_ligature:"
					  "deminutum head must have different "
					  "pitch -> ignoring grob");
	  }
      else if (prefix_set & (CAVUM | LINEA))
	if ((prefix_set & CAVUM) && (prefix_set & LINEA))
	  glyph_name = "vaticana.linea.punctum.cavum";
	else if (prefix_set & CAVUM)
	  glyph_name = "vaticana.punctum.cavum";
	else
	  glyph_name = "vaticana.linea.punctum";
      else if (prefix_set & AUCTUM)
	if (prefix_set & ASCENDENS)
	  glyph_name = "solesmes.auct.asc";
	else
	  glyph_name = "solesmes.auct.desc";
      else if ((context_info & STACKED_HEAD)
	       && (context_info & PES_UPPER))
	if (prev_delta_pitch > 1)
	  glyph_name = "vaticana.upes";
	else
	  glyph_name = "vaticana.vupes";
      else
	glyph_name = "vaticana.punctum";

      /*
       * This head needs a cauda, if it starts a flexa, is not the upper
       * head of a pes, and if it is a punctum.
       */
      if ((context_info & FLEXA_LEFT) && ! (context_info & PES_UPPER))
	if (!String::compare (glyph_name, "vaticana.punctum"))
	  primitive->set_property ("add-cauda", ly_bool2scm (true));

      /*
       * Execptional rule for porrectus:
       *
       * If the current head is preceded by a \flexa and succeded by a
       * \pes (e.g. "a \flexa g \pes a"), then join the current head and
       * the previous head into a single curved flexa shape.
       */
      if ((context_info & FLEXA_RIGHT) && (context_info & PES_LOWER))
	{
	  check_for_prefix_loss (prev_primitive);
	  prev_glyph_name = "flexa";
	  prev_primitive->set_property ("flexa-height",
					scm_from_int (prev_delta_pitch));
	  prev_primitive->set_property ("flexa-width",
					scm_from_double (flexa_width));
	  bool add_cauda = !(prev_prefix_set && PES_OR_FLEXA);
	  prev_primitive->set_property ("add-cauda",
					ly_bool2scm (add_cauda));
	  check_for_prefix_loss (primitive);
	  glyph_name = "";
	  primitive->set_property ("flexa-width",
				   scm_from_double (flexa_width));
	}

      /*
       * Exceptional rule for pes:
       *
       * If this head is stacked on the previous one due to a \pes, then
       * set the glyph of the previous head to that for this special
       * case, thereby avoiding potential vertical collision with the
       * current head.
       */
      if (prefix_set & PES_OR_FLEXA)
	{
	  if ((context_info & PES_UPPER) && (context_info & STACKED_HEAD))
	    {
	      if (!String::compare (prev_glyph_name, "vaticana.punctum"))
		if (prev_delta_pitch > 1)
		  prev_glyph_name = "vaticana.lpes";
		else
		  prev_glyph_name = "vaticana.vlpes";
	    }
	}

      if (prev_primitive)
	prev_primitive->set_property ("glyph-name",
				      scm_makfrom0str (prev_glyph_name.to_str0 ()));

      /*
       * In the backend, flexa shapes and joins need to know about line
       * thickness.  Hence, for simplicity, let's distribute the
       * ligature grob's value for thickness to each ligature head (even
       * if not all of them need to know).
       */
      primitive->set_property ("thickness", scm_from_double (thickness));

      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
      prev_context_info = context_info;
      prev_delta_pitch = delta_pitch;
      prev_glyph_name = glyph_name;
    }

  prev_primitive->set_property ("glyph-name",
				scm_makfrom0str (prev_glyph_name.to_str0 ()));

  align_heads (primitives, flexa_width, thickness);

#if 0 // experimental code to collapse spacing after ligature
  /* TODO: set to max (old/new spacing-increment), since other
     voices/staves also may want to set this property. */
  Item *first_primitive = dynamic_cast<Item *> (primitives[0].grob ());
  Paper_column *paper_column = first_primitive->get_column ();
  paper_column->warning (_f ("Vaticana_ligature_engraver: "
			     "setting `spacing-increment = %f': ptr =%ul",
			     ligature_width, paper_column));
  paper_column->
    set_property ("forced-spacing", scm_from_double (ligature_width));
#endif
}

ADD_ACKNOWLEDGER (Vaticana_ligature_engraver, rest);
ADD_ACKNOWLEDGER (Vaticana_ligature_engraver, note_head);
ADD_TRANSLATOR (Vaticana_ligature_engraver,
		/* doc */ "Handles ligatures by glueing special ligature heads together.",
		/* create */ "VaticanaLigature",
		/* accept */ "ligature-event",
		/* read */ "",
		/* write */ "");
