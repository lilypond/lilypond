/*
  score-engraver.cc -- implement Score_engraver

  source file of the GNU LilyPond music typesetter

  (c) 1997--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score-engraver.hh"

#include "all-font-metrics.hh"
#include "axis-group-interface.hh"
#include "context-def.hh"
#include "dispatcher.hh"
#include "global-context.hh"
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

IMPLEMENT_LISTENER (Score_engraver, prepare);
void
Score_engraver::prepare (SCM)
{
  precomputed_recurse_over_translators (context (), START_TRANSLATION_TIMESTEP, DOWN);
}

IMPLEMENT_LISTENER (Score_engraver, finish);
void
Score_engraver::finish (SCM)
{
  recurse_over_translators (context (), &Translator::finalize,
			    &Translator_group::finalize,
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
      error (_f ("cannot find `%s'", MUSIC_FONT ".otf")
	     + "\n"
	     + _ ("Music font has not been installed properly.")
	     + "\n"
	     + _f ("Search path `%s'", global_path.to_string ().c_str ())
	     + "\n"
	     + _ ("Aborting"));
    }

  pscore_ = new Paper_score (dynamic_cast<Output_def *> (context ()->get_output_def ()));
  pscore_->unprotect ();
  context ()->set_property ("output", pscore_->self_scm ());

  SCM props = updated_grob_properties (context (), ly_symbol2scm ("System"));

  pscore_->typeset_system (new System (props));
  
  system_ = pscore_->root_system ();
  context ()->set_property ("rootSystem", system_->self_scm ());

  Engraver_group::initialize ();
}

void
Score_engraver::connect_to_context (Context *c)
{
  Engraver_group::connect_to_context (c);
  
  Dispatcher *d = c->get_global_context ()->event_source ();
  d->add_listener (GET_LISTENER (one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->add_listener (GET_LISTENER (prepare), ly_symbol2scm ("Prepare"));
  d->add_listener (GET_LISTENER (finish), ly_symbol2scm ("Finish"));
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
  Dispatcher *d = context ()->get_global_context ()->event_source ();
  d->remove_listener (GET_LISTENER (one_time_step), ly_symbol2scm ("OneTimeStep"));
  d->remove_listener (GET_LISTENER (prepare), ly_symbol2scm ("Prepare"));
  d->remove_listener (GET_LISTENER (finish), ly_symbol2scm ("Finish"));

  Engraver_group::disconnect_from_context ();
}

void
Score_engraver::finalize ()
{
  Engraver_group::finalize ();

  typeset_all ();
}

IMPLEMENT_LISTENER (Score_engraver, one_time_step);
void
Score_engraver::one_time_step (SCM)
{
  if (!to_boolean (context ()->get_property ("skipTypesetting")))
    {
      precomputed_recurse_over_translators (context (), PROCESS_MUSIC, UP);
      Engraver_group::do_announces ();
    }

  precomputed_recurse_over_translators (context (), STOP_TRANSLATION_TIMESTEP, UP);
  typeset_all ();
}

void
Score_engraver::announce_grob (Grob_info info)
{
  Engraver_group::announce_grob (info);
  if (info.start_end () == START)
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
		      "currentCommandColumn "
		      "verticallySpacedContexts ",

		      /* write */
		      ""
		      );
