/*
  gregorian-ligature-engraver.cc -- implement Gregorian_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2008 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "gregorian-ligature-engraver.hh"

#include "gregorian-ligature.hh"
#include "international.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

/* ASSIGN_EVENT_ONCE */
#include "translator.icc"

/*
 * This abstract class is the common superclass for all ligature
 * engravers for Gregorian chant notation.  It cares for the musical
 * handling of the neumes, such as checking for valid combinations of
 * neumes and providing context information.  Notational aspects such
 * as the glyphs to use or calculating the total width of a ligature,
 * are left to the concrete subclass.  Currently, there is only a
 * single subclass, Vaticana_ligature_engraver.  Other ligature
 * engravers for Gregorian chant will be added in the future, such as
 * Medicaea_ligature_engraver or Hufnagel_ligature_engraver.
 */
Gregorian_ligature_engraver::Gregorian_ligature_engraver ()
{
  pes_or_flexa_req_ = 0;
}

void
Gregorian_ligature_engraver::listen_pes_or_flexa (Stream_event *ev)
{
  ASSIGN_EVENT_ONCE (pes_or_flexa_req_, ev);
}

void fix_prefix (char const *name, int mask,
		 int *current_set, int min_set, int max_set,
		 Grob *primitive)
{
  bool current = *current_set & mask;
  bool min = min_set & mask;
  bool max = max_set & mask;
  if (max < min)
    {
      programming_error ("min_set > max_set");
      return;
    }
  if (min && !current)
    {
      primitive->warning (_f ("\\%s ignored", name));
      *current_set &= ~mask;
    }
  if (!max && current)
    {
      primitive->warning (_f ("implied \\%s added", name));
      *current_set |= mask;
    }
}

void fix_prefix_set (int *current_set, int min_set, int max_set, Grob *primitive)
{
  fix_prefix ("virga", VIRGA, current_set, min_set, max_set, primitive);
  fix_prefix ("stropha", STROPHA, current_set, min_set, max_set, primitive);
  fix_prefix ("inclinatum", INCLINATUM, current_set, min_set, max_set, primitive);
  fix_prefix ("auctum", AUCTUM, current_set, min_set, max_set, primitive);
  fix_prefix ("descendens", DESCENDENS, current_set, min_set, max_set, primitive);
  fix_prefix ("ascendens", ASCENDENS, current_set, min_set, max_set, primitive);
  fix_prefix ("oriscus", ORISCUS, current_set, min_set, max_set, primitive);
  fix_prefix ("quilisma", QUILISMA, current_set, min_set, max_set, primitive);
  fix_prefix ("deminutum", DEMINUTUM, current_set, min_set, max_set, primitive);
  fix_prefix ("cavum", CAVUM, current_set, min_set, max_set, primitive);
  fix_prefix ("linea", LINEA, current_set, min_set, max_set, primitive);
  fix_prefix ("pes_or_flexa", LINEA, current_set, min_set, max_set, primitive);
}

void check_and_fix_all_prefixes (vector<Grob_info> primitives)
{
  /* Check for invalid head modifier combinations */
  for (vsize i = 0; i < primitives.size (); i++)
    {
      Grob *primitive = primitives[i].grob ();

      /* compute head prefix set by inspecting primitive grob properties */
      int prefix_set
	= (VIRGA *to_boolean (primitive->get_property ("virga")))
	| (STROPHA *to_boolean (primitive->get_property ("stropha")))
	| (INCLINATUM *to_boolean (primitive->get_property ("inclinatum")))
	| (AUCTUM *to_boolean (primitive->get_property ("auctum")))
	| (DESCENDENS *to_boolean (primitive->get_property ("descendens")))
	| (ASCENDENS *to_boolean (primitive->get_property ("ascendens")))
	| (ORISCUS *to_boolean (primitive->get_property ("oriscus")))
	| (QUILISMA *to_boolean (primitive->get_property ("quilisma")))
	| (DEMINUTUM *to_boolean (primitive->get_property ("deminutum")))
	| (CAVUM *to_boolean (primitive->get_property ("cavum")))
	| (LINEA *to_boolean (primitive->get_property ("linea")))
	| (PES_OR_FLEXA *to_boolean (primitive->get_property ("pes-or-flexa")));

      /* check: ascendens and descendens exclude each other; same with
	 auctum and deminutum */
      if (prefix_set & DESCENDENS)
	{
	  fix_prefix_set (&prefix_set,
			  prefix_set & ~ASCENDENS,
			  prefix_set & ~ASCENDENS,
			  primitive);
	}
      if (prefix_set & AUCTUM)
	{
	  fix_prefix_set (&prefix_set,
			  prefix_set & ~DEMINUTUM,
			  prefix_set & ~DEMINUTUM,
			  primitive);
	}

      /* check: virga, quilisma and oriscus cannot be combined with any
	 other prefix, but may be part of a pes or flexa */
      if (prefix_set & VIRGA)
	{
	  fix_prefix_set (&prefix_set,
			  VIRGA,
			  VIRGA | PES_OR_FLEXA,
			  primitive);
	}
      if (prefix_set & QUILISMA)
	{
	  fix_prefix_set (&prefix_set,
			  QUILISMA,
			  QUILISMA | PES_OR_FLEXA,
			  primitive);
	}
      if (prefix_set & ORISCUS)
	{
	  fix_prefix_set (&prefix_set,
			  ORISCUS,
			  ORISCUS | PES_OR_FLEXA,
			  primitive);
	}

      /* check: auctum is the only valid optional prefix for stropha */
      if (prefix_set & STROPHA)
	{
	  fix_prefix_set (&prefix_set,
			  STROPHA,
			  STROPHA | AUCTUM,
			  primitive);
	}

      /* check: inclinatum may be prefixed with auctum or deminutum only */
      if (prefix_set & INCLINATUM)
	{
	  fix_prefix_set (&prefix_set,
			  INCLINATUM,
			  INCLINATUM | AUCTUM | DEMINUTUM,
			  primitive);
	}
      /* check: semivocalis (deminutum but not inclinatum) must occur in
	 combination with and only with pes or flexa */
      else if (prefix_set & DEMINUTUM)
	{
	  fix_prefix_set (&prefix_set,
			  DEMINUTUM | PES_OR_FLEXA,
			  DEMINUTUM | PES_OR_FLEXA,
			  primitive);
	}

      /* check: cavum and linea (either or both) may be applied only
	 upon core punctum */
      if (prefix_set & (CAVUM | LINEA))
	{
	  fix_prefix_set (&prefix_set,
			  0,
			  CAVUM | LINEA,
			  primitive);
	}

      /* all other combinations should be valid (unless I made a
	 mistake) */

      primitive->set_property ("prefix-set", scm_from_int (prefix_set));
    }
}

/*
 * Marks those heads that participate in a pes or flexa.
 */
void
provide_context_info (vector<Grob_info> primitives)
{
  Grob *prev_primitive = 0;
  int prev_prefix_set = 0;
  int prev_context_info = 0;
  int prev_pitch = 0;
  for (vsize i = 0; i < primitives.size (); i++)
    {
      Grob *primitive = primitives[i].grob ();
      Stream_event *event_cause = primitives[i].event_cause ();
      int context_info = 0;
      int pitch = unsmob_pitch (event_cause->get_property ("pitch"))->steps ();
      int prefix_set = scm_to_int (primitive->get_property ("prefix-set"));

      if (prefix_set & PES_OR_FLEXA)
	{
	  if (!i) // ligature may not start with 2nd head of pes or flexa
	    primitive->warning (_ ("cannot apply `\\~' on first head of ligature"));
	  else if (pitch > prev_pitch) // pes
	    {
	      prev_context_info |= PES_LOWER;
	      context_info |= PES_UPPER;
	    }
	  else if (pitch < prev_pitch) // flexa
	    {
	      prev_context_info |= FLEXA_LEFT;
	      context_info |= FLEXA_RIGHT;
	    }
	  else // (pitch == prev_pitch)
	    primitive->warning (_ ("cannot apply `\\~' on heads with identical pitch"));
	}
      if (prev_prefix_set & DEMINUTUM)
	context_info |= AFTER_DEMINUTUM;

      if (prev_primitive)
	prev_primitive->set_property ("context-info",
				      scm_from_int (prev_context_info));
      prev_primitive = primitive;
      prev_prefix_set = prefix_set;
      prev_context_info = context_info;
      prev_pitch = pitch;
    }
  if (prev_primitive)
    prev_primitive->set_property ("context-info",
				  scm_from_int (prev_context_info));
}

void
Gregorian_ligature_engraver::build_ligature (Spanner *ligature,
					     vector<Grob_info> primitives)
{
  // apply style-independent checking and transformation
  check_and_fix_all_prefixes (primitives);
  provide_context_info (primitives);

  // apply style-specific transformation (including line-up); to be
  // implemented by subclass
  transform_heads (ligature, primitives);
}

void
Gregorian_ligature_engraver::stop_translation_timestep ()
{
  Ligature_engraver::stop_translation_timestep ();
  pes_or_flexa_req_ = 0;
}

// no ADD_ACKNOWLEDGER / ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
