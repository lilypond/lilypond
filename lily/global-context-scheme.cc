/*
  global-context-scheme.cc -- implement Global_context bindings

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
#include "translator-group.hh"
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

  if (Music_output *od = unsmob_music_output (output))
    od->process ();
  
  return output;
}

LY_DEFINE (ly_make_global_translator, "ly:make-global-translator",
          1, 0, 0, (SCM global),
          "Create a translator group and connect it to the global context\n"
          "@var{global}. The translator group is returned.")
{
  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (global));
  SCM_ASSERT_TYPE (g, global, SCM_ARG1, __FUNCTION__, "Global context");

  Translator_group *tg = new Translator_group ();
  tg->connect_to_context (g);
  g->implementation_ = tg;

  return tg->unprotect ();
}

LY_DEFINE (ly_make_global_context, "ly:make-global-context",
	   1, 1, 0, (SCM output_def, SCM key),
	   "Set up a global interpretation context, using the output\n"
	   "block @var{output_def}.\n"
	   "The context is returned.\n"

	   "\n\nOptionally, this routine takes an Object-key to\n"
	   "to uniquely identify the Score block containing it.\n")
{
  Output_def *odef = unsmob_output_def (output_def);

  SCM_ASSERT_TYPE (odef, output_def, SCM_ARG1, __FUNCTION__,
		   "Output definition");

  Global_context *glob = new Global_context (odef, unsmob_key (key));

  if (!glob)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }

  return glob->unprotect ();
}

LY_DEFINE (ly_interpret_music_expression, "ly:interpret-music-expression",
	   2, 0, 0, (SCM mus, SCM ctx),
	   "Interpret the music expression @var{mus} in the\n"
	   "global context @var{ctx}. The context is returned in its\n"
	   "final state.\n")
{
  Music *music = unsmob_music (mus);
  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (ctx));
  SCM_ASSERT_TYPE (music, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (g, ctx, SCM_ARG2, __FUNCTION__, "Global context");

  if (!music
      || !music->get_length ().to_bool ())
    {
      warning (_ ("no music found in score"));
      return SCM_BOOL_F;
    }

  Cpu_timer timer;

  message (_ ("Interpreting music... "));

  SCM protected_iter = Music_iterator::get_static_get_iterator (music);
  Music_iterator *iter = unsmob_iterator (protected_iter);

  iter->init_context (music, g);
  iter->construct_children ();

  if (!iter->ok ())
    {
      warning (_ ("no music found in score"));
      /* todo: should throw exception. */
      return SCM_BOOL_F;
    }

  g->run_iterator_on_me (iter);

  iter->quit ();
  scm_remember_upto_here_1 (protected_iter);

  send_stream_event (g, "Finish", 0, 0);

  if (be_verbose_global)
    message (_f ("elapsed time: %.2f seconds", timer.read ()));

  return ctx;
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
  SCM glob = ly_make_global_context (output_def, key);
  ly_make_global_translator (glob);
  ly_interpret_music_expression (mus, glob);
  return glob;
}
