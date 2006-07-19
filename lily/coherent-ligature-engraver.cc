/*
  coherent-ligature-engraver.cc -- implement Coherent_ligature_engraver

  source file of the GNU LilyPond music typesetter

  (c) 2003--2006 Juergen Reuter <reuter@ipd.uka.de>
*/

#include "coherent-ligature-engraver.hh"

#include "warn.hh"
#include "staff-symbol-referencer.hh"
#include "spanner.hh"
#include "paper-column.hh"
#include "pitch.hh"
#include "pointer-group-interface.hh"

/*
 * This abstract class serves as common superclass for all ligature
 * engravers thet produce a single connected graphical object of fixed
 * width, consisting of noteheads and other primitives (see class
 * Ligature_engraver for more information on the interaction between
 * this class and its superclass).  In particular, it cares for the
 * following tasks:
 *
 * - provide a function for putting all grobs of the ligature into a
 * single paper column,
 *
 * - delegate actual creation of ligature to concrete subclass,
 *
 * - collect all accidentals that occur within the ligature and put
 * them at the left side of the ligature (TODO; see function
 * collect_accidentals ()),
 *
 * - collapse superflous space after each ligature (TODO).
 *
 * Concrete subclasses must implement function build_ligature (Spanner
 * *, vector<Grob_info>).  This function is responsible for actually
 * building the ligature by transforming the array of noteheads.
 *
 * Currently, there are two subclasses: Gregorian_ligature_engraver
 * for Gregorian chant notation (also known as plain song or cantus
 * planus) and Mensural_ligature_engraver for white mensural notation.
 * Subclasses for other music notation styles such as modal notation
 * or ars nova notation may eventually be added.
 */

/*
 * TODO: local accidentals: collect accidentals that occur within a
 * ligature and put them before the ligature.  If an accidental
 * changes within a ligature, print a warning (user error) and ignore
 * any further accidental for that pitch within that ligature
 * (actually, in such a case, the user should split the ligature into
 * two separate ligatures).  Similarly, any object that, in ordinary
 * notation, may be put to the left or to the right of a note-head,
 * should be collected and put before or after the ligature.
 *
 * TODO: make spacing more robust: do not screw up spacing if user
 * erroneously puts rest in ligature.
 *
 * TODO: for each ligature, add Rod that represents the total length
 * of the ligature (to preemptively avoid collision with adjacent
 * notes); or maybe just additionally create a
 * mensural/vaticana/whatever-ligature grob (e.g. via
 * Mensural_ligature::print (SCM)) that just consists of a
 * bounding box around all primitives of the ligature.
 *
 * TODO: Maybe move functions fold_up_primitives () and
 * join_primitives () from subclasses to here?  N.B. it is not
 * appropriate to put these into Ligature_engraver, since, for
 * example, Ligature_bracket_engraver does not share any of this code.
 */

/*
 * TODO: Let superflous space after each ligature collapse.  The
 * following code should help in doing so (though it does not yet
 * fully work).  Just put the following code into
 * Spacing_spanner::do_measure ().  I put it temporarily here as memo
 * until it really works and I also get Han-Wen's/Jan's permission to
 * add it to the spacing spanner code.
 */
#if 0 /* experimental code to collapse spacing after ligature */
SCM incr_scm = lc->get_property ("forced-spacing");
if (incr_scm != SCM_EOL) /* (Paper_column::is_musical (l)) */
  {
    me->warning (_f ("gotcha: ptr=%ul", lc));//debug
    ly_display_scm (lc->self_scm ());
    Real distance;
    if (incr_scm != SCM_EOL)
      distance = scm_to_double (incr_scm);
    else
      {
	me->warning (_ ("distance undefined, assuming 0.1"));
	distance = 0.1;
      }
    me->warning (_f ("distance=%f", distance));//debug
    Real inverse_strength = 1.0;
    Spaceable_grob::add_spring (lc, rc, distance, inverse_strength);
    if (Item *rb = r->find_prebroken_piece (LEFT))
      Spaceable_grob::add_spring (lc, rb, distance, inverse_strength);

    continue;
  }
#endif

/*
 * TODO: move this function to class Item?
 */
void
Coherent_ligature_engraver::get_set_column (Item *item, Paper_column *column)
{
  Item *parent = dynamic_cast<Item *> (item->get_parent (X_AXIS));
  if (!parent)
    {
      programming_error ("failed tweaking paper column in ligature");
      return;
    }

  string name = parent->name ();
  if (name != "PaperColumn")
    {
      // Change column not only for targeted item (NoteColumn), but
      // also for all associated grobs (NoteSpacing, SeparationItem).
      Grob *sl = Staff_symbol_referencer::get_staff_symbol (item);

      extract_item_set (parent, "elements", elements);

      for (vsize i = elements.size (); i--;)
	{
	  Item *sibling = elements[i];
	  if ((sibling)
	      && (Staff_symbol_referencer::get_staff_symbol (sibling) == sl))
	    {
#if 0 /* experimental code to collapse spacing after ligature */
	      Grob *sibling_parent = sibling->get_parent (X_AXIS);
	      sibling_parent->warning (_f ("Coherent_ligature_engraver: "
					   "setting `spacing-increment="
					   "0.01': ptr=%ul", parent));
	      sibling_parent->set_property ("forced-spacing",
					    scm_from_double (0.01));
#endif
	      sibling->set_parent (column, X_AXIS);
	    }
	}
    }
  else
    get_set_column (parent, column);
}

/*
 * TODO: This function should collect all accidentals that occur
 * within the ligature (by scanning through the primitives array) and
 * place all of them at the left of the ligature.  If there is an
 * alteration within the ligature (e.g. an "f" followed by a "fis"
 * somewhere later in the ligature), issue a warning (and maybe create
 * an additional natural symbol to explicitly make clear that there is
 * an "f" first?).  The warning should suggest the user to break the
 * ligature into two or more smaller ligatures such that no alteration
 * occurs within the broken ligatures any more.
 */
void
Coherent_ligature_engraver::collect_accidentals (Spanner *, vector<Grob_info>)
{
  /* TODO */
}

void
compute_delta_pitches (vector<Grob_info> primitives)
{
  int prev_pitch = 0;
  int delta_pitch = 0;
  Item *prev_primitive = 0, *primitive = 0;
  for (vsize i = 0; i < primitives.size (); i++)
    {
      primitive = dynamic_cast<Item *> (primitives[i].grob ());
      Music *music_cause = primitives[i].music_cause ();
      int pitch
	= unsmob_pitch (music_cause->get_property ("pitch"))->steps ();
      if (prev_primitive)
	{
	  delta_pitch = pitch - prev_pitch;
	  prev_primitive->set_property ("delta-position",
					scm_from_int (delta_pitch));
	}
      prev_pitch = pitch;
      prev_primitive = primitive;
    }
  primitive->set_property ("delta-position", scm_from_int (0));
}

void
Coherent_ligature_engraver::typeset_ligature (Spanner *ligature,
					      vector<Grob_info> primitives)
{
  // compute some commonly needed context info stored as grob
  // properties
  compute_delta_pitches (primitives);

  // prepare ligature for typesetting
  build_ligature (ligature, primitives);
  collect_accidentals (ligature, primitives);
}

// no ADD_ACKNOWLEDGER / ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
