/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "score-engraver.hh"

#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "context-def.hh"
#include "context.hh"
#include "dispatcher.hh"
#include "grob-properties.hh"
#include "international.hh"
#include "main.hh"
#include "open-type-font.hh"
#include "output-def.hh"
#include "paper-column-engraver.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "system.hh"
#include "warn.hh"

Score_engraver::Score_engraver ()
{
  system_ = 0;
  pscore_ = 0;
}

void
Score_engraver::derived_mark () const
{
  if (pscore_)
    scm_gc_mark (pscore_->self_scm ());
  Engraver_group::derived_mark ();
}

void Score_engraver::prepare (SCM)
{
  precomputed_recurse_over_translators (context (), START_TRANSLATION_TIMESTEP,
                                        DOWN);
}

void Score_engraver::finish (SCM)
{
  recurse_over_translators (
      context (),
      Callback0_wrapper::make_smob<Translator, &Translator::finalize> (),
      Callback0_wrapper::make_smob<Translator_group,
                                   &Translator_group::finalize> (),
      UP);
}

#define MUSIC_FONT "emmentaler-20"

/*
  use start/finish?
*/
void
Score_engraver::initialize ()
{
  Font_metric *fm = all_fonts_global->find_otf (MUSIC_FONT);
  if (!fm)
    {
      error (_f ("cannot find `%s'", MUSIC_FONT ".otf") + "\n"
             + _ ("Music font has not been installed properly.") + "\n"
             + _f ("Search path `%s'", global_path.to_string ().c_str ()) + "\n"
             + _ ("Aborting"));
    }

  pscore_ = new Paper_score (
      dynamic_cast<Output_def *> (context ()->get_output_def ()));
  pscore_->unprotect ();
  context ()->set_property ("output", pscore_->self_scm ());

  SCM props
      = Grob_property_info (context (), ly_symbol2scm ("System")).updated ();

  pscore_->typeset_system (new System (props));

  system_ = pscore_->root_system ();
  context ()->set_property ("rootSystem", system_->self_scm ());

  Engraver_group::initialize ();
}

void
Score_engraver::connect_to_context (Context *c)
{
  Engraver_group::connect_to_context (c);

  Dispatcher *d = find_top_context (c)->event_source ();
  d->add_listener (GET_LISTENER (Score_engraver, one_time_step),
                   ly_symbol2scm ("OneTimeStep"));
  d->add_listener (GET_LISTENER (Score_engraver, prepare),
                   ly_symbol2scm ("Prepare"));
  d->add_listener (GET_LISTENER (Score_engraver, finish),
                   ly_symbol2scm ("Finish"));
}

/*
  uncovered:

  check_removal always returns false for Score contexts, it has been that way
since I joined the project. There is a reason for this: The typeset score is
stored in the Score_engraver, which in turn is accessed through the
Global_context returned by ly:run-translator. So the score-translator must be
connected to the score-context after run-translator finishes.

I plan to change this: we should junk run-translator, and instead keep track
of both context and translator in the SCM code, and access the typeset score
directly via the created global-translator. Then it would be possible to
disconnect score-translators at iteration time. -es
 */
void
Score_engraver::disconnect_from_context ()
{
  Dispatcher *d = find_top_context (context ())->event_source ();
  d->remove_listener (GET_LISTENER (Score_engraver, one_time_step),
                      ly_symbol2scm ("OneTimeStep"));
  d->remove_listener (GET_LISTENER (Score_engraver, prepare),
                      ly_symbol2scm ("Prepare"));
  d->remove_listener (GET_LISTENER (Score_engraver, finish),
                      ly_symbol2scm ("Finish"));

  Engraver_group::disconnect_from_context ();
}

void
Score_engraver::finalize ()
{
  Engraver_group::finalize ();

  typeset_all ();
}

void Score_engraver::one_time_step (SCM)
{
  if (!to_boolean (context ()->get_property ("skipTypesetting")))
    {
      precomputed_recurse_over_translators (context (), PROCESS_MUSIC, UP);
      Engraver_group::do_announces ();
    }

  precomputed_recurse_over_translators (context (), STOP_TRANSLATION_TIMESTEP,
                                        UP);
  typeset_all ();
}

void
Score_engraver::announce_grob (Grob_info info, Direction start_end,
                               Context *reroute_context)
{
  Engraver_group::announce_grob (info, start_end, reroute_context);
  if (start_end == START)
    {
      pscore_->root_system ()->typeset_grob (info.grob ());
      elems_.push_back (info.grob ());
    }
}

void
Score_engraver::typeset_all ()
{
  for (vsize i = 0; i < elems_.size (); i++)
    {
      Grob *elem = elems_[i];

      if (!elem->get_parent (Y_AXIS))
        Axis_group_interface::add_element (system_, elem);
    }
  elems_.clear ();
}

ADD_TRANSLATOR_GROUP (Score_engraver,
                      /* doc */
                      "The top-level engraver.  Takes care of generating"
                      " columns and the complete system (i.e.,"
                      " @code{System}).\n"
                      "\n"
                      "This engraver decides whether a column is breakable."
                      "  The default is that a column is always breakable."
                      "  However, every @code{Bar_engraver} that does not have"
                      " a bar line at a certain point sets @code{forbidBreaks}"
                      " to stop line breaks.  In practice, this means that you"
                      " can make a break point by creating a bar line"
                      " (assuming that there are no beams or notes that"
                      " prevent a break point).",

                      /* create */
                      "System ",

                      /* read */
                      "currentMusicalColumn "
                      "currentCommandColumn ",

                      /* write */
                      "");
