/*
  gregorian-ligature-engraver.cc -- implement Gregorian_ligature_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (C) 2003 Juergen Reuter <reuter@ipd.uka.de>
 */

#include "gregorian-ligature-engraver.hh"
#include "gregorian-ligature.hh"
#include "item.hh"
#include "warn.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"
#include "paper-column.hh"

/*
 * TODO: This class shares some code with Mensural_ligature_engraver.
 * Maybe we should create a common super class "Rod_ligature_engraver"
 * and derive all shared code from it.
 */

Gregorian_ligature_engraver::Gregorian_ligature_engraver ()
{
  porrectus_req_ = 0;
}

void
Gregorian_ligature_engraver::transform_heads (Spanner *, Array<Grob_info>)
{
  programming_error ("Gregorian_ligature_engraver::transform_heads (): "
		     "this is an abstract method that should not be called, "
		     "but overridden by a subclass");
}

bool
Gregorian_ligature_engraver::try_music (Music *m)
{
  if (m->is_mus_type ("porrectus-event"))
    {
      porrectus_req_ = m;
      return true;
    }
  else
    return Ligature_engraver::try_music (m);
}

/*
 * TODO: move this function to class Item?
 */
void
Gregorian_ligature_engraver::get_set_column (Item *item, Paper_column *column)
{
  Item *parent = dynamic_cast<Item*> (item->get_parent (X_AXIS));
  if (!parent)
    {
      programming_error ("failed tweaking paper column in ligature");
      return;
    }

  String name = parent->name ();
  if (!String::compare (name, "PaperColumn"))
    {
      // Change column not only for targeted item (NoteColumn), but
      // also for all associated grobs (NoteSpacing, SeparationItem).
      Grob *sl = Staff_symbol_referencer::get_staff_symbol (item);
      for (SCM tail = parent->get_grob_property ("elements");
	   gh_pair_p (tail);
	   tail = ly_cdr (tail))
	{
	  Item *sibling = unsmob_item (ly_car (tail));
	  if ((sibling) &&
	      (Staff_symbol_referencer::get_staff_symbol (sibling) == sl))
	    {
	      sibling->set_parent (column, X_AXIS);
	    }
	}
    }
  else
    {
      get_set_column (parent, column);
    }
}

void fix_prefix (char *name, int mask,
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
  fix_prefix ("deminutus", DEMINUTUM, current_set, min_set, max_set, primitive);
  fix_prefix ("semivocalis", SEMIVOCALIS, current_set, min_set, max_set, primitive);
  fix_prefix ("cavum", CAVUM, current_set, min_set, max_set, primitive);
  fix_prefix ("linea", LINEA, current_set, min_set, max_set, primitive);
  fix_prefix ("pes_or_flexa", LINEA, current_set, min_set, max_set, primitive);
}

void check_and_fix_all_prefixes (Array<Grob_info> primitives)
{
  /* Check for illegal head modifier combinations */
  for (int i = 0; i < primitives.size(); i++)
    {
    Grob *primitive = primitives[i].grob_;

    /* compute head prefix set by inspecting primitive grob properties */
    int prefix_set =
      (VIRGA * to_boolean (primitive->get_grob_property ("virga"))) |
      (STROPHA * to_boolean (primitive->get_grob_property ("stropha"))) |
      (INCLINATUM * to_boolean (primitive->get_grob_property ("inclinatum"))) |
      (AUCTUM * to_boolean (primitive->get_grob_property ("auctum"))) |
      (DESCENDENS * to_boolean (primitive->get_grob_property ("descendens"))) |
      (ASCENDENS * to_boolean (primitive->get_grob_property ("ascendens"))) |
      (ORISCUS * to_boolean (primitive->get_grob_property ("oriscus"))) |
      (QUILISMA * to_boolean (primitive->get_grob_property ("quilisma"))) |
      (DEMINUTUM * to_boolean (primitive->get_grob_property ("deminutum"))) |
      (SEMIVOCALIS * to_boolean (primitive->get_grob_property ("semivocalis"))) |
      (CAVUM * to_boolean (primitive->get_grob_property ("cavum"))) |
      (LINEA * to_boolean (primitive->get_grob_property ("linea"))) |
      (PES_OR_FLEXA * to_boolean (primitive->get_grob_property ("pes-or-flexa")));

    /* check: ascendens and descendens exclude each other; same with
       auctum and diminutum */
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

    /* check: virga, quilisma and oriscus can not be combined with any
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

    /* check: semivocalis must occur in combination with and only with
       pes or flexa */
    if (prefix_set & SEMIVOCALIS)
      {
	fix_prefix_set (&prefix_set,
			SEMIVOCALIS | PES_OR_FLEXA,
			SEMIVOCALIS | PES_OR_FLEXA,
			primitive);
      }

    /* check: inclinatum may be prefixed with auctum or diminutum only */
    if (prefix_set & INCLINATUM)
      {
	fix_prefix_set (&prefix_set,
			INCLINATUM,
			INCLINATUM | AUCTUM | DEMINUTUM,
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

    primitive->set_grob_property ("prefix-set", gh_int2scm (prefix_set));
  }
}

/*
 * Marks those heads that participate in a pes or flexa.
 */
void
provide_context_info (Array<Grob_info> primitives)
{
  Grob *prev_primitive = 0;
  int prev_context_info = 0;
  int prev_pitch = 0;
  for (int i = 0; i < primitives.size(); i++) {
    Grob *primitive = primitives[i].grob_;
    Music *music_cause = primitives[i].music_cause ();
    int context_info = 0;
    int pitch = unsmob_pitch (music_cause->get_mus_property ("pitch"))->steps ();
    int prefix_set = gh_scm2int (primitive->get_grob_property ("prefix-set"));

    if (prefix_set & PES_OR_FLEXA)
      if (pitch > prev_pitch) // pes
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
	{
	  primitive->warning ("may not apply `\\~' on heads with "
			      "identical pitch; ignoring `\\~'");
	}
    if (prev_primitive)
      prev_primitive->set_grob_property ("context-info",
					 gh_int2scm (prev_context_info));
    prev_primitive = primitive;
    prev_context_info = context_info;
    prev_pitch = pitch;
  }
  if (prev_primitive)
    prev_primitive->set_grob_property ("context-info",
				       gh_int2scm (prev_context_info));
}

void
Gregorian_ligature_engraver::typeset_ligature (Spanner *ligature,
					       Array<Grob_info> primitives)
{
  // apply style-independent checking and transformation
  check_and_fix_all_prefixes (primitives);
  provide_context_info (primitives);

  // apply style-specific transformation (including line-up)
  transform_heads (ligature, primitives);

  // typeset
  for (int i = 0; i < primitives.size (); i++)
    {
      typeset_grob (primitives[i].grob_);
    }
}

void
Gregorian_ligature_engraver::start_translation_timestep ()
{
  Ligature_engraver::start_translation_timestep ();
  porrectus_req_ = 0;
}

ENTER_DESCRIPTION (Gregorian_ligature_engraver,
/* descr */       "This is an abstract class.  Subclasses such as Vaticana_ligature_engraver handle ligatures by glueing special ligature heads together.",
/* creats*/       "",
/* accepts */     "ligature-event abort-event",
/* acks  */      "ligature-head-interface note-head-interface rest-interface",
/* reads */       "",
/* write */       "");
