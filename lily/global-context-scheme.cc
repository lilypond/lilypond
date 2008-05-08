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
#include "output-def.hh"
#include "translator-group.hh"
#include "warn.hh"

LY_DEFINE (ly_format_output, "ly:format-output",
	   1, 0, 0, (SCM context),
	   "Given a global context in its final state,"
	   " process it and return the @code{Music_output} object"
	   " in its final state.")
{
  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (context));
  
  LY_ASSERT_TYPE (unsmob_global_context, context, 1)

  SCM output = g->get_output ();
  progress_indication ("\n");

  if (Music_output *od = unsmob_music_output (output))
    od->process ();
  
  return output;
}

LY_DEFINE (ly_make_global_translator, "ly:make-global-translator",
          1, 0, 0, (SCM global),
          "Create a translator group and connect it to the global context"
          " @var{global}.  The translator group is returned.")
{
  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (global));
  LY_ASSERT_TYPE (unsmob_global_context, global, 1)

  Translator_group *tg = new Translator_group ();
  tg->connect_to_context (g);
  g->implementation_ = tg;

  return tg->unprotect ();
}

LY_DEFINE (ly_make_global_context, "ly:make-global-context",
	   1, 0, 0, (SCM output_def),
	   "Set up a global interpretation context, using the output"
	   " block @var{output_def}.  The context is returned.")
{
  LY_ASSERT_SMOB (Output_def, output_def, 1);
  Output_def *odef = unsmob_output_def (output_def); 

  Global_context *glob = new Global_context (odef);

  if (!glob)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }

  return glob->unprotect ();
}

LY_DEFINE (ly_interpret_music_expression, "ly:interpret-music-expression",
	   2, 0, 0, (SCM mus, SCM ctx),
	   "Interpret the music expression @var{mus} in the global context"
	   " @var{ctx}.  The context is returned in its final state.")
{
  LY_ASSERT_SMOB (Music, mus, 1);
  LY_ASSERT_TYPE (unsmob_global_context, ctx, 2);

  Music *music = unsmob_music (mus);
  if (!music
      || !music->get_length ().to_bool ())
    {
      warning (_ ("no music found in score"));
      return SCM_BOOL_F;
    }

  Global_context *g = dynamic_cast<Global_context *> (unsmob_context (ctx));

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
	   2, 1, 0, (SCM mus, SCM output_def),
	   "Process @var{mus} according to @var{output-def}.  An"
	   " interpretation context is set up, and @var{mus} is"
	   " interpreted with it.  The context is returned in its"
	   " final state.\n"
	   "\n"
	   "Optionally, this routine takes an object-key to"
	   " to uniquely identify the score block containing it.")
{
  LY_ASSERT_SMOB (Music, mus, 1);
  LY_ASSERT_SMOB (Output_def, output_def, 2);

  SCM glob = ly_make_global_context (output_def);
  ly_make_global_translator (glob);
  ly_interpret_music_expression (mus, glob);
  return glob;
}
