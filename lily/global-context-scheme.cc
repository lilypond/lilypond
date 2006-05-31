/*
  global-context-scheme.cc -- implement Global_context bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2006 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "cpu-timer.hh"
#include "global-context.hh"
#include "international.hh"
#include "main.hh"
#include "music-iterator.hh"
#include "music-output.hh"
#include "music.hh"
#include "object-key.hh"
#include "output-def.hh"
#include "warn.hh"

LY_DEFINE (ly_format_output, "ly:format-output",
	   1, 0, 0, (SCM context),
	   "Given a Global context in its final state, "
	   "process it and return the @code{Music_output} object in its final state.")
{
  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (context));
  SCM_ASSERT_TYPE (g, context, SCM_ARG1, __FUNCTION__, "Global context");

  SCM output = g->get_output ();
  progress_indication ("\n");
  unsmob_music_output (output)->process ();
  return output;
}

LY_DEFINE (ly_run_translator, "ly:run-translator",
	   2, 1, 0, (SCM mus, SCM output_def, SCM key),
	   "Process @var{mus} according to @var{output_def}. \n"
	   "An interpretation context is set up,\n"
	   "and @var{mus} is interpreted with it.  \n"
	   "The context is returned in its final state.\n"
	   "\n\n"
	   "Optionally, this routine takes an Object-key to\n"
	   "to uniquely identify the Score block containing it.\n")
{
  Output_def *odef = unsmob_output_def (output_def);
  Music *music = unsmob_music (mus);

  if (!music
      || !music->get_length ().to_bool ())
    {
      warning (_ ("no music found in score"));
      return SCM_BOOL_F;
    }

  SCM_ASSERT_TYPE (music, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (odef, output_def, SCM_ARG2, __FUNCTION__,
		   "Output definition");

  Cpu_timer timer;

  Global_context *trans = new Global_context (odef, music->get_length (),
					      unsmob_key (key));
  if (!trans)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }

  message (_ ("Interpreting music... "));

  SCM protected_iter = Music_iterator::get_static_get_iterator (music);
  Music_iterator *iter = unsmob_iterator (protected_iter);
  iter->init_translator (music, trans);
  iter->construct_children ();

  if (!iter->ok ())
    {
      warning (_ ("no music found in score"));
      /* todo: should throw exception. */
      return SCM_BOOL_F;
    }

  trans->run_iterator_on_me (iter);
  iter->quit ();
  scm_remember_upto_here_1 (protected_iter);
  trans->finish ();

  if (be_verbose_global)
    message (_f ("elapsed time: %.2f seconds", timer.read ()));

  return trans->unprotect ();
}
