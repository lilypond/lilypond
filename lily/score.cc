/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "ly-smobs.icc"

#include "score.hh"
#include "warn.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "music-iterator.hh"
#include "music.hh"
#include "global-context.hh"
#include "scm-hash.hh"
#include "cpu-timer.hh"
#include "main.hh"
#include "paper-def.hh"
#include "ly-modules.hh"



/*
  TODO: junkme.
 */
Score::Score ()
  : Input ()
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;

  smobify_self ();
}

Score::~Score ()
{
  
}




IMPLEMENT_SMOBS (Score);
IMPLEMENT_DEFAULT_EQUAL_P (Score);


SCM
Score::mark_smob (SCM s)
{
  Score * sc = (Score*) SCM_CELL_WORD_1 (s);

  if (sc->header_)
    scm_gc_mark (sc->header_);
  for (int i = sc->defs_.size (); i--;)
    scm_gc_mark (sc->defs_[i]->self_scm ());
  
  return sc->music_;
}

int
Score::print_smob (SCM , SCM p, scm_print_state*)
{
  scm_puts ("#<Score>", p);

  return 1;
}



/*
  store point & click locations.
  Global to save some time. (Sue us!)
 */

Score::Score (Score const &s)
  : Input (s)
{
  music_ = SCM_EOL;
  header_ = 0;
  smobify_self ();

  Music * m =unsmob_music (s.music_);
  music_ =  m?m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i=0; i < s.defs_.size (); i++)
    defs_.push (s.defs_[i]->clone ());

  header_ = ly_make_anonymous_module ();
  if (ly_module_p (s.header_))
    ly_copy_module_variables (header_, s.header_);
}



LY_DEFINE (ly_run_translator, "ly:run-translator", 
	  2, 0, 0,
	  (SCM mus, SCM output_def),
	  "Process @var{mus} according to @var{output_def}. A interpretation "
"context is set up, and @var{mus} is interpreted with it. The  "
"context is returned in its final state." )
{
  Music_output_def *odef = unsmob_music_output_def (output_def);
  Music * music = unsmob_music (mus);

  SCM_ASSERT_TYPE (music, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (odef, output_def, SCM_ARG2, __FUNCTION__, "Output definition");
  
  Cpu_timer timer;
  
  Global_context * trans = new Global_context (odef);
  
  if (!trans)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }
  progress_indication (_ ("Interpreting music..."));
  
  trans->final_mom_ = music->get_length ();
  SCM protected_iter =  Music_iterator::get_static_get_iterator (music);
  Music_iterator * iter = unsmob_iterator (protected_iter);
  iter->init_translator (music, trans);

  iter->construct_children ();

  if (! iter->ok ())
    {
      warning (_ ("Need music in a score"));
      return SCM_BOOL_F;	// todo: shoudl throw exception.
    }

  trans->run_iterator_on_me (iter);
  iter->quit ();
  scm_remember_upto_here_1 (protected_iter);
  trans->finish ();

  if (verbose_global_b)
    progress_indication (_f ("elapsed time: %.2f seconds",  timer.read ()));

  
  return scm_gc_unprotect_object (trans->self_scm ());
}


LY_DEFINE (ly_render_output, "ly:render-output",
	  3,0,0,
	  (SCM context, SCM header, SCM out_filename),
	  "Given a Score context in its final state, calculate the output, "
	  "and  dump the result to @var{out-filename}, using "
	  "@var{header} for the bibliographic information.")
{
  Global_context * gt = dynamic_cast<Global_context *> (unsmob_context (context));
  
  SCM_ASSERT_TYPE (gt, context, SCM_ARG1, __FUNCTION__,
		  "Global context");
  SCM_ASSERT_TYPE (ly_module_p (header), header, SCM_ARG2, __FUNCTION__,
		  "module");
  SCM_ASSERT_TYPE (gh_string_p (out_filename), out_filename, SCM_ARG3, __FUNCTION__,
		  "output filename");

  Music_output * output = gt->get_output ();

  output->header_ = header;
  
  progress_indication ("\n");
  output->process (ly_scm2string (out_filename));
  
  delete output ;

  return SCM_UNDEFINED ;
}

void
default_rendering (SCM mus, SCM outdef, SCM head, SCM outname)
{
  SCM context = ly_run_translator (mus, outdef);
  
  if (unsmob_context (context))
    ly_render_output (context,  head, outname);
}

