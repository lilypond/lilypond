/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2022 Juergen Reuter <reuter@ipd.uka.de>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "ligature-engraver.hh"

#include "context.hh"
#include "international.hh"
#include "item.hh"
#include "note-head.hh"
#include "rest.hh"
#include "spanner.hh"
#include "stream-event.hh"
#include "translator.icc"

/*
 * This abstract class provides the general framework for ligatures of
 * any kind.  It cares for handling start/stop ligatures events and
 * collecting all noteheads inbetween, but delegates creation of a
 * ligature spanner for each start/stop pair and typesetting of the
 * ligature spanner to a concrete subclass.
 *
 * A concrete ligature engraver must subclass this class and provide
 * functions create_ligature_spanner () and typeset_ligature
 * (Spanner *, vector<Grob_info>).  Subclasses of this class basically
 * fall into two categories.
 *
 * The first category consists of engravers that engrave ligatures in
 * a way that really deserves the name ligature.  That is, they
 * produce a single connected graphical object of fixed width,
 * consisting of noteheads and other primitives.  Space may be
 * inserted only after each ligature, if necessary, but in no case
 * between the primitives of the ligature. The same approach is
 * used for Kievan notation ligatures, or, rather melismas.
 * Though these are not single connected objects, they behave much
 * in the same way and have a fixed, small amount of space between
 * noteheads. Except in Kievan "ligatures", accidentals have to be put
 * to the left of the ligature, and not to the left of individual
 * noteheads. In Kievan ligatures, the B-flat may be part of the
 * ligature itself. Class Coherent_ligature_engraver is the common
 * superclass for all of these engravers.
 *
 * The second category is for engravers that are relaxed in the sense
 * that they do not require to produce a single connected graphical
 * object.  For example, in contemporary editions, ligatures are often
 * marked, but otherwise use contemporary notation and spacing.  In
 * this category, there is currently only a single class,
 * Ligature_bracket_engraver, which marks each ligature with a
 * horizontal sqare bracket, but otherwise leaves the appearance
 * untouched.
 */

/*
 * TODO: lyrics/melisma/syllables: there should be at most one
 * syllable of lyrics per ligature (i.e. for the lyrics context, a
 * ligature should count as a single note, regardless of how many
 * heads the ligature consists of).
 *
 * TODO: currently, you have to add/remove the proper
 * Ligature_engraver (Ligature_bracket_engraver,
 * Mensural_ligature_engraver) to the proper translator
 * (e.g. VoiceContext) to choose between various representations.
 * Since adding/removing an engraver to a translator is a global
 * action in the layout block, you cannot mix various representations
 * _within_ the same score.  Hence, for selecting a representation,
 * one would rather like to have a property that can be set e.g. for
 * several staves individually.  However, it seems that this approach
 * would require to have a single, complicated Ligature_engraver that
 * consists of all the code...  This needs further thoughts.
 */
Ligature_engraver::Ligature_engraver (Context *c)
  : Engraver (c)
{
}

void
Ligature_engraver::pre_process_music ()
{
  if (ligature_ && !ligature_listener_.get_stop ())
    set_property (find_score_context (), "forbidBreak", SCM_BOOL_T);
}

void
Ligature_engraver::process_music ()
{
  if (auto *const ender = ligature_listener_.get_stop ())
    {
      if (!ligature_)
        {
          ender->warning (_ ("cannot find start of ligature"));
          return;
        }

      if (!last_bound_)
        ender->warning (_ ("no right bound"));
      else
        ligature_->set_bound (RIGHT, last_bound_);

      finished_primitives_ = primitives_;
      finished_ligature_ = ligature_;
      primitives_.clear ();
      ligature_ = nullptr;
    }
  last_bound_ = unsmob<Grob> (get_property (this, "currentMusicalColumn"));

  if (auto *const starter = ligature_listener_.get_start ())
    {
      if (ligature_)
        {
          starter->warning (_ ("already have a ligature"));
          ligature_->warning (_ ("ligature was started here"));
          return;
        }

      ligature_ = create_ligature_spanner ();

      Grob *bound = unsmob<Grob> (get_property (this, "currentMusicalColumn"));
      if (!bound)
        starter->warning (_ ("no left bound"));
      else
        ligature_->set_bound (LEFT, bound);

      ligature_start_mom_ = now_mom ();

      // TODO: dump cause into make_item/spanner.
      // announce_grob (ligature_, starter->self_scm ());
    }
}

void
Ligature_engraver::stop_translation_timestep ()
{
  if (finished_ligature_)
    {
      if (!finished_primitives_.size ())
        {
          finished_ligature_->programming_error (
            "Ligature_engraver::stop_translation_timestep ():"
            " junking empty ligature");
        }
      else
        {
          typeset_ligature (finished_ligature_, finished_primitives_);
          finished_primitives_.clear ();
        }
      finished_ligature_ = nullptr;
    }

  ligature_listener_.reset ();
}

void
Ligature_engraver::finalize ()
{
  if (finished_ligature_)
    {
      typeset_ligature (finished_ligature_, finished_primitives_);
      finished_primitives_.clear ();
      finished_ligature_ = nullptr;
    }
  if (ligature_)
    {
      ligature_->warning (_ ("unterminated ligature"));
      ligature_->suicide ();
    }
}

Spanner *
Ligature_engraver::current_ligature ()
{
  return ligature_;
}

void
Ligature_engraver::acknowledge_ligature_head (Grob_info_t<Item> info)
{
  if (ligature_)
    {
      primitives_.push_back (info.grob ());
      if (info.grob () && !scm_is_null (brew_ligature_primitive_proc))
        set_property (info.grob (), "stencil", brew_ligature_primitive_proc);
    }
}

void
Ligature_engraver::acknowledge_rest (Grob_info info)
{
  if (ligature_)
    {
      info.event_cause ()->warning (
        _ ("ignoring rest: ligature may not contain rest"));
      ligature_->warning (_ ("ligature was started here"));
      // TODO: maybe better should stop ligature here rather than
      // ignoring the rest?
    }
}

// no ADD_ACKNOWLEDGER / ADD_ACKNOWLEDGER / ADD_TRANSLATOR macro calls
// since this class is abstract
