/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "lily-parser.hh"
#include "book.hh"
#include "cpu-timer.hh"
#include "global-context.hh"
#include "ly-module.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "music-iterator.hh"
#include "output-def.hh"
#include "music-output.hh"
#include "music.hh"
#include "paper-book.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "warn.hh"

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
IMPLEMENT_TYPE_P (Score, "ly:score?");

SCM
Score::mark_smob (SCM s)
{
  Score *sc = (Score*) SCM_CELL_WORD_1 (s);
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

Score::Score (Score const &s)
  : Input (s)
{
  music_ = SCM_EOL;

  /* FIXME: SCM_EOL? */
  header_ = 0;

  smobify_self ();

  Music *m =unsmob_music (s.music_);
  music_ = m ? m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i = 0; i < s.defs_.size (); i++)
    defs_.push (s.defs_[i]->clone ());

  header_ = ly_make_anonymous_module (false);
  if (ly_c_module_p (s.header_))
    ly_import_module (header_, s.header_);
}


LY_DEFINE (ly_run_translator, "ly:run-translator", 
	   2, 0, 0, (SCM mus, SCM output_def),
	   "Process @var{mus} according to @var{output_def}. "
	   "An interpretation context is set up, "
	   "and @var{mus} is interpreted with it.  "
	   "The context is returned in its final state.")
{
  Output_def *odef = unsmob_output_def (output_def);
  Music *music = unsmob_music (mus);

  if (!music
      || !music->get_length ().to_bool ())
    {
      warning (_ ("Need music in a score"));
      return SCM_BOOL_F;
    }
  
  SCM_ASSERT_TYPE (music, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (odef, output_def, SCM_ARG2, __FUNCTION__, "Output definition");
  
  Cpu_timer timer;
  
  Global_context *trans = new Global_context (odef, music->get_length ());
  
  if (!trans)
    {
      programming_error ("no toplevel translator");
      return SCM_BOOL_F;
    }
  progress_indication (_ ("Interpreting music... "));
  
  SCM protected_iter = Music_iterator::get_static_get_iterator (music);
  Music_iterator * iter = unsmob_iterator (protected_iter);
  iter->init_translator (music, trans);

  iter->construct_children ();

  if (!iter->ok ())
    {
      warning (_ ("Need music in a score"));
      /* todo: should throw exception. */
      return SCM_BOOL_F;
    }

  trans->run_iterator_on_me (iter);
  iter->quit ();
  scm_remember_upto_here_1 (protected_iter);
  trans->finish ();

  if (verbose_global_b)
    progress_indication (_f ("elapsed time: %.2f seconds",  timer.read ()));
  
  return scm_gc_unprotect_object (trans->self_scm ());
}

LY_DEFINE (ly_format_output, "ly:format-output",
	   2, 0, 0, (SCM context, SCM outname),
	   "Given a Score context in its final state,"
           "process it and return the (rendered) result.")
{
  Global_context *g = dynamic_cast<Global_context*> (unsmob_context (context));
  SCM_ASSERT_TYPE (g, context, SCM_ARG1, __FUNCTION__, "Global context");
  SCM_ASSERT_TYPE (scm_is_string (outname), outname, SCM_ARG2, __FUNCTION__, "output filename");

  Music_output *output = g->get_output ();
  progress_indication ("\n");
  /* ugh, midi still wants outname  */
  return output->process (ly_scm2string (outname));
}

void
default_rendering (SCM music, SCM outdef,
		   SCM book_outputdef,
		   SCM header, SCM outname)
{
  SCM scaled_def = outdef;
  SCM scaled_bookdef = book_outputdef;
  
  Output_def *bpd = unsmob_output_def (book_outputdef);

  /* ugh.  */
  if (bpd->c_variable ("is-bookpaper") == SCM_BOOL_T)
    {
      Real scale = scm_to_double (bpd->c_variable ("outputscale"));
      
      Output_def *def = scale_output_def (unsmob_output_def (outdef), scale);
      scaled_def = def->self_scm ();

      scaled_bookdef = scale_output_def (bpd, scale)->self_scm ();
      unsmob_output_def (scaled_def)->parent_
	= unsmob_output_def (scaled_bookdef);
      
      scm_gc_unprotect_object (scaled_bookdef);
      scm_gc_unprotect_object (scaled_def);
    }
  
  SCM context = ly_run_translator (music, scaled_def);
  if (Global_context *g = dynamic_cast<Global_context*>
      (unsmob_context (context)))
    {
      SCM systems = ly_format_output (context, outname);
      Music_output *output = g->get_output ();
      if (systems != SCM_UNDEFINED)
	{
	  /* ugh, this is strange, Paper_book without a Book object. */
	  Paper_book *paper_book = new Paper_book ();
	  paper_book->header_ = header;
	  paper_book->bookpaper_ = unsmob_output_def (scaled_bookdef);
	  
	  Score_systems sc;
	  sc.systems_ = systems;
	  sc.header_ = header;

	  paper_book->score_systems_.push (sc);
	  
	  paper_book->classic_output (ly_scm2string (outname));
	  scm_gc_unprotect_object (paper_book->self_scm ());
	}
      delete output;
    }

  scm_remember_upto_here_1 (scaled_def);
  scm_remember_upto_here_1 (scaled_bookdef);
}

/*
Format score, return systems. OUTNAME is still passed to create a midi
file.

PAPERBOOK should be scaled already.

*/
SCM
Score::book_rendering (String outname,
		       Output_def *paperbook,
		       Output_def *default_def)
{
  SCM scaled_bookdef = SCM_EOL;
  Real scale = 1.0;

  if (paperbook && paperbook->c_variable ("is-bookpaper") == SCM_BOOL_T)
    scale = scm_to_double (paperbook->c_variable ("outputscale"));
  
  SCM out = scm_makfrom0str (outname.to_str0 ());
  SCM systems = SCM_EOL;
  int outdef_count = defs_.size ();
  for (int i = 0; !i || i < outdef_count; i++)
    {
      Output_def *def = outdef_count ? defs_[i] : default_def;
      SCM scaled= SCM_EOL;
      if (def->c_variable ("is-paper") == SCM_BOOL_T)
	{
	  def = scale_output_def (def, scale);
	  def->parent_ = paperbook;
	  scaled = def->self_scm ();
	  scm_gc_unprotect_object (scaled);
	}

      /* TODO: fix or junk --no-paper.  */
      SCM context = ly_run_translator (music_, def->self_scm ());
      if (dynamic_cast<Global_context*> (unsmob_context (context)))
	{
	  SCM s = ly_format_output (context, out);
	  if (s != SCM_UNDEFINED)
	    systems = s;
	}

      scm_remember_upto_here_1 (scaled);
    }
  
  scm_remember_upto_here_1 (scaled_bookdef);
  return systems;
}




LY_DEFINE (ly_score_embedded_format, "ly:score-embedded-format",
	   2, 0, 0, (SCM score, SCM paper),
	   "Run @var{score} through @var{paper}, an output definition, "
	   "scaled to correct outputscale already, "
	   "return a list of paper-lines.")
{
  Score * sc = unsmob_score (score);
  Output_def *od = unsmob_output_def (paper);

  SCM_ASSERT_TYPE (sc, score, SCM_ARG1, __FUNCTION__, "Score");
  SCM_ASSERT_TYPE (od, paper, SCM_ARG2, __FUNCTION__, "Output_def");

  Output_def * score_def  = 0;

  /* UGR, FIXME, these are default \paper blocks once again.  They
     suck. */
  for (int i = 0; !score_def && i < sc->defs_.size (); i++)
    if (sc->defs_[i]->c_variable ("is-paper") == SCM_BOOL_T)
      score_def = sc->defs_[i];

  if (!score_def)
    return scm_c_make_vector (0, SCM_EOL);
      
  score_def = score_def->clone ();
  SCM prot = score_def->self_scm ();
  scm_gc_unprotect_object (prot);

  /* TODO: SCORE_DEF should be scaled according to OD->parent_ or OD
     itself. */
  score_def->parent_ = od;
  
  SCM context = ly_run_translator (sc->get_music (), score_def->self_scm ());
  SCM lines = ly_format_output (context, scm_makfrom0str ("<embedded>"));
  
  scm_remember_upto_here_1 (prot);
  return lines;
}

void
Score::set_music (SCM music, SCM parser)
{
  /* URG? */
  SCM check_funcs = ly_scheme_function ("toplevel-music-functions");
  for (; ly_c_pair_p (check_funcs); check_funcs = ly_cdr (check_funcs))
    music = scm_call_2 (ly_car (check_funcs), music, parser);

  if (unsmob_music (music_))
    {
      unsmob_music (music)->origin ()->error (_("Already have music in score"));
      unsmob_music (music_)->origin ()->error (_("This is the previous music"));
    }
	
  this->music_ = music;
}

SCM
Score::get_music () const
{
  return music_;
}
