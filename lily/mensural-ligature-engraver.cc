/*
  mensural-ligature-engraver.cc -- implement Mensural_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2002--2008 Juergen Reuter <reuter@ipd.uka.de>,
  Pal Benko <benkop@freestart.hu>
*/

#include "coherent-ligature-engraver.hh"
#include "font-interface.hh"
#include "international.hh"
#include "mensural-ligature.hh"
#include "note-column.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "rhythmic-head.hh"
#include "spanner.hh"
#include "staff-symbol-referencer.hh"
#include "stream-event.hh"
#include "warn.hh"

#include "translator.icc"

/*
 * TODO: accidentals are aligned with the first note;
 * they must appear ahead.
 *
 * TODO: prohibit ligatures having notes differing only in accidentals
 * (like \[ a\breve g as \])
 *
 * TODO: do something with multiple voices within a ligature.  See
 * for example:
 * Ockeghem: Missa Ecce ancilla domini, bassus part, end of Christe.
 *
 * TODO: enhance robustness: in case of an invalid ligature (e.g. the
 * input specifies a ligature that contains a minima), automatically
 * break the ligature into smaller, valid pieces.  Such a piece may be
 * a single note.
 */

class Mensural_ligature_engraver : public Coherent_ligature_engraver
{

protected:
  virtual Spanner *create_ligature_spanner ();
  virtual void build_ligature (Spanner *ligature, vector<Grob_info> primitives);
  DECLARE_TRANSLATOR_LISTENER (ligature);
  
public:
  TRANSLATOR_DECLARATIONS (Mensural_ligature_engraver);

private:
  void transform_heads (vector<Grob_info> primitives);
  void propagate_properties (Spanner *ligature, vector<Grob_info> primitives);
  void fold_up_primitives (vector<Grob_info> primitives);
};

IMPLEMENT_TRANSLATOR_LISTENER (Mensural_ligature_engraver, ligature);
void
Mensural_ligature_engraver::listen_ligature (Stream_event *ev)
{
  Ligature_engraver::listen_ligature (ev);
}

Mensural_ligature_engraver::Mensural_ligature_engraver ()
{
  brew_ligature_primitive_proc = 
    Mensural_ligature::brew_ligature_primitive_proc;
}

Spanner *
Mensural_ligature_engraver::create_ligature_spanner ()
{
  return make_spanner ("MensuralLigature", SCM_EOL);
}

void
Mensural_ligature_engraver::transform_heads (vector<Grob_info> primitives)
{
  if (primitives.size () < 2)
    {
      warning (_ ("ligature with less than 2 heads -> skipping"));
      return;
    }
  int prev_pitch = 0;
  bool at_beginning = true;

  // needed so that we can check whether
  // the previous note can be turned into a flexa
  bool prev_brevis_shape = false;

  bool prev_semibrevis = false;
  Item *prev_primitive = NULL;

  for (vsize i = 0, s = primitives.size (); i < s; i++)
    {
      Grob_info info = primitives[i];
      Item *primitive = dynamic_cast<Item *> (info.grob ());
      int duration_log = Rhythmic_head::duration_log (primitive);

      Stream_event *nr = info.event_cause ();

      /*
	ugh. why not simply check for pitch?
      */
      if (!nr->in_event_class ("note-event"))
	{
	  nr->origin ()->warning
	    (_ ("cannot determine pitch of ligature primitive -> skipping"));
	  at_beginning = true;
	  continue;
	}

      int pitch = unsmob_pitch (nr->get_property ("pitch"))->steps ();
      int delta_pitch = 0;

      if (at_beginning)
	{
	  if (i == s - 1)
	    {
	      // we can get here after invalid input
	      nr->origin ()->warning
		(_ ("single note ligature - skipping"));
	      break;
	    }
	  prev_semibrevis = prev_brevis_shape = false;
	  prev_primitive = NULL;
	}
      else
	{
	  delta_pitch = pitch - prev_pitch;
	  if (delta_pitch == 0)
	    {
	      nr->origin ()->warning
		(_ ("prime interval within ligature -> skipping"));
	      at_beginning = true;
	      primitive->set_property ("primitive",
				       scm_from_int (MLP_NONE));
	      continue;
	    }
	}

      if (duration_log < -3 // is this possible at all???
	  || duration_log > 0)
	{
	  nr->origin ()->warning
	    (_ ("mensural ligature: duration none of Mx, L, B, S -> skipping"));
	  primitive->set_property ("primitive",
				   scm_from_int (MLP_NONE));
	  at_beginning = true;
	  continue;
	}

      // apply_transition replacement begins
      bool general_case = true;

      // first check special cases
      // 1. beginning
      if (at_beginning)
	{
	  // a. semibreves
	  if (duration_log == 0)
	    {
	      primitive->set_property ("primitive",
				       scm_from_int (MLP_UP | MLP_BREVIS));
	      prev_semibrevis = prev_brevis_shape = true;
	      general_case = false;
	    }
	  // b. descendens longa or brevis
	  else if (i < s - 1
		   && (unsmob_pitch (primitives[i + 1].event_cause ()
				     ->get_property ("pitch"))->steps () < pitch)
		   && duration_log > -3)
	    {
	      int left_stem = duration_log == -1 ? MLP_DOWN : 0;

	      primitive->set_property ("primitive",
				       scm_from_int (left_stem | MLP_BREVIS));
	      prev_brevis_shape = true;
	      prev_semibrevis = general_case = false;
	    }
	}
      // 2. initial semibrevis must be followed by another one
      else if (prev_semibrevis)
	{
	  prev_semibrevis = false;
	  if (duration_log == 0)
	    {
	      primitive->set_property ("primitive", scm_from_int (MLP_BREVIS));
	      general_case = false;
	    }
	  else
	    {
	      nr->origin ()->warning
		(_ ("semibrevis must be followed by another one -> skipping"));
	      primitive->set_property ("primitive",
				       scm_from_int (MLP_NONE));
	      at_beginning = true;
	      continue;
	    }
	}
      // 3. semibreves are otherwise not allowed
      else if (duration_log == 0)
	{
	  nr->origin ()->warning
	    (_ ("semibreves can only appear at the beginning of a ligature,\n"
		"and there may be only zero or two of them"));
	  primitive->set_property ("primitive",
				   scm_from_int (MLP_NONE));
	  at_beginning = true;
	  continue;
	}
      // 4. end, descendens
      else if (i == s - 1 && delta_pitch < 0)
	{
	  // brevis; previous note must be turned into flexa
	  if (duration_log == -1)
	    {
	      if (prev_brevis_shape)
		{
		  prev_primitive->set_property
		    ("primitive",
		     scm_from_int
		     (MLP_FLEXA
		      | (scm_to_int (prev_primitive->get_property ("primitive"))
			 & MLP_DOWN)));
		  primitive->set_property ("primitive", scm_from_int (MLP_NONE));
		  break; // no more notes, no join
		}
	      else
		{
		  nr->origin ()->warning
		    (_ ("invalid ligatura ending:\n"
			"when the last note is a descending brevis,\n"
			"the penultimate note must be another one,\n"
			"or the ligatura must be LB or SSB"));
		  primitive->set_property ("primitive", scm_from_int (MLP_NONE));
		  break;
		}
	    }
	  // longa
	  else if (duration_log == -2)
	    {
	      primitive->set_property ("primitive", scm_from_int (MLP_BREVIS));
	      general_case = false;
	    }
	  // else maxima; fall through regular case below
	}

      if (general_case)
	{
	  static int const shape[3] = {MLP_MAXIMA, MLP_LONGA, MLP_BREVIS};

	  primitive->set_property ("primitive",
				   scm_from_int (shape[duration_log + 3]));
	  prev_brevis_shape = duration_log == -1;
	}

      // join_primitives replacement
      if (!at_beginning)
	{
	  /*
	    if the previous note is longa-shaped and this note is lower,
	    then the joining line may hide the stem, so it is made longer
	    to serve as stem as well
	  */
	  if (delta_pitch < 0
	      && (scm_to_int (prev_primitive->get_property ("primitive"))
		  & MLP_LONGA))
	    {
	      delta_pitch -= 6;
	      // instead of number 6
	      // the legth of the longa stem should be queried something like
	      // Font_interface::get_default_font (ligature)->find_by_name
	      //  ("noteheads.sM2mensural").extent (Y_AXIS).length ()
	    }
	  prev_primitive->set_property ("join-right-amount",
					scm_from_int (delta_pitch));
	  // perhaps set add-join as well
	}
      at_beginning = false;
      prev_primitive = primitive;
      prev_pitch = pitch;
      // apply_transition replacement ends
    }
}

/*
 * A MensuralLigature grob consists of a bunch of NoteHead grobs that
 * are glued together.  It (a) does not make sense to change
 * properties like thickness or flexa-width from one head to the next
 * within a ligature (this would totally screw up alignment), and (b)
 * some of these properties (like flexa-width) are specific to
 * e.g. the MensuralLigature (as in contrast to e.g. LigatureBracket),
 * and therefore should not be handled in the NoteHead code (which is
 * also used by LigatureBracket).  Therefore, we let the user control
 * these properties via the concrete Ligature grob (like
 * MensuralLigature) and then copy these properties as necessary to
 * each of the NoteHead grobs.  This is what
 * propagate_properties () does.
 */
void
Mensural_ligature_engraver::propagate_properties (Spanner *ligature,
						  vector<Grob_info> primitives)
{
  Real thickness
    = robust_scm2double (ligature->get_property ("thickness"), 1.4);
  thickness
    *= ligature->layout ()->get_dimension (ly_symbol2scm ("line-thickness"));

  Real head_width
    = Font_interface::get_default_font (ligature)->
    find_by_name ("noteheads.sM1mensural").extent (X_AXIS).length ();
  Real flexa_width
    = robust_scm2double (ligature->get_property ("flexa-width"), 2);
  Real maxima_head_width
    = Font_interface::get_default_font (ligature)->
    find_by_name ("noteheads.sM1neomensural").extent (X_AXIS).length ();

  flexa_width *= Staff_symbol_referencer::staff_space (ligature);

  Real half_flexa_width = 0.5 * (flexa_width + thickness);

  for (vsize i = 0; i < primitives.size (); i++)
    {
      Item *primitive = dynamic_cast<Item *> (primitives[i].grob ());
      int output = scm_to_int (primitive->get_property ("primitive"));
      primitive->set_property ("thickness",
			       scm_from_double (thickness));

      switch (output & MLP_ANY)
	{
	case MLP_NONE:
	  primitive->set_property ("head-width",
				   scm_from_double (half_flexa_width));
	  break;
	case MLP_BREVIS:
	case MLP_LONGA:
	  primitive->set_property ("head-width",
				   scm_from_double (head_width));
	  break;
	case MLP_MAXIMA:
	  primitive->set_property ("head-width",
				   scm_from_double (maxima_head_width));
	  break;
	case MLP_FLEXA:
	  primitive->set_property ("head-width",
				   scm_from_double (half_flexa_width));
	  primitive->set_property ("flexa-width",
				   scm_from_double (flexa_width));
	  break;
	default:
	  programming_error (_ ("unexpected case fall-through"));
	  break;
	}
    }
}

void
Mensural_ligature_engraver::fold_up_primitives (vector<Grob_info> primitives)
{
  Item *first = 0;
  Real distance = 0.0;
  Real dot_shift = 0.0;
  for (vsize i = 0; i < primitives.size (); i++)
    {
      Item *current = dynamic_cast<Item *> (primitives[i].grob ());
      if (i == 0)
	{
	  first = current;
	  dot_shift = 1.5 * Staff_symbol_referencer::staff_space (first);
	}

      move_related_items_to_column (current, first->get_column (),
				    distance);

      distance
	+= scm_to_double (current->get_property ("head-width"))
	- scm_to_double (current->get_property ("thickness"));

      if (Rhythmic_head::dot_count (current) > 0)
	// Move dots above/behind the ligature.
	{
	  if (i + 1 < primitives.size ())
	    // dot in the midst => move above head
	    {
	      // FIXME: Amount of vertical dot-shift should depend on
	      // pitch.
	      //
	      // FIXME: dot placement is horizontally slightly off.
	      Rhythmic_head::get_dots (current)->translate_axis (dot_shift, Y_AXIS);
	    }
	  else
	    // trailing dot => move behind head
	    {
	      double head_width =
		scm_to_double (current->get_property ("head-width"));
	      Rhythmic_head::get_dots (current)->
		translate_axis (head_width, X_AXIS);
	    }
	}
    }
}

void
Mensural_ligature_engraver::build_ligature (Spanner *ligature,
					    vector<Grob_info> primitives)
{
  transform_heads (primitives);
  propagate_properties (ligature, primitives);
  fold_up_primitives (primitives);
}

ADD_ACKNOWLEDGER (Mensural_ligature_engraver, rest);
ADD_ACKNOWLEDGER (Mensural_ligature_engraver, note_head);

ADD_TRANSLATOR (Mensural_ligature_engraver,
		/* doc */
		"Handle @code{Mensural_ligature_events} by glueing special"
		" ligature heads together.",

		/* create */
		"MensuralLigature ",

		/* read */
		"",

		/* write */
		""
		);
