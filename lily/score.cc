/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include <stdio.h>

#include "book.hh"
#include "book-paper-def.hh"
#include "cpu-timer.hh"
#include "global-context.hh"
#include "ly-module.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "music-iterator.hh"
#include "music-output-def.hh"
#include "music-output.hh"
#include "music.hh"
#include "paper-book.hh"
#include "paper-def.hh"
#include "paper-score.hh"
#include "scm-hash.hh"
#include "score.hh"
#include "warn.hh"

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



/*
  store point & click locations.
  Global to save some time. (Sue us!)
 */

Score::Score (Score const &s)
  : Input (s)
{
  music_ = SCM_EOL;

  // FIXME: SCM_EOL?
  header_ = 0;

  smobify_self ();

  Music *m =unsmob_music (s.music_);
  music_ = m ? m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i = 0; i < s.defs_.size (); i++)
    defs_.push (s.defs_[i]->clone ());

  header_ = ly_make_anonymous_module (false);
  if (is_module (s.header_))
    ly_import_module (header_, s.header_);
}

LY_DEFINE (ly_run_translator, "ly:run-translator", 
	  2, 0, 0, (SCM mus, SCM output_def),
	   "Process @var{mus} according to @var{output_def}. "
	   "An interpretation context is set up, "
	   "and @var{mus} is interpreted with it.  "
	   "The context is returned in its final state.")
{
  Music_output_def *odef = unsmob_music_output_def (output_def);
  Music *music = unsmob_music (mus);

  SCM_ASSERT_TYPE (music, mus, SCM_ARG1, __FUNCTION__, "Music");
  SCM_ASSERT_TYPE (odef, output_def, SCM_ARG2, __FUNCTION__, "Output definition");
  
  Cpu_timer timer;
  
  Global_context * trans = new Global_context (odef, music->get_length ());
  
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
  SCM_ASSERT_TYPE (ly_c_string_p (outname), outname, SCM_ARG2, __FUNCTION__, "output filename");

  Music_output *output = g->get_output ();
  progress_indication ("\n");
  // ugh, midi still wants outname
  return output->process (ly_scm2string (outname));
}

void
default_rendering (SCM music, SCM outdef,
		   SCM book_outputdef,
		   SCM header, SCM outname)
{
  SCM context = ly_run_translator (music, outdef);

  Book_paper_def *bpd = unsmob_book_paper_def (book_outputdef);
  if (bpd && unsmob_paper (outdef))
    /* FIXME:  memory leak */
    outdef = bpd->scale_paper (unsmob_paper (outdef))->self_scm ();
  
  if (Global_context *g = dynamic_cast<Global_context*>
      (unsmob_context (context)))
    {
      SCM systems = ly_format_output (context, outname);
      Music_output *output = g->get_output ();
      if (systems != SCM_UNDEFINED)
	{
	  Paper_book *paper_book = new Paper_book ();
	  Paper_score *ps = dynamic_cast<Paper_score*> (output);

	  Score_lines sc;
	  sc.paper_ = ps->paper_;
	  sc.lines_ = systems;
	  sc.header_ = header;

	  paper_book->score_lines_.push (sc);
	  
	  paper_book->classic_output (ly_scm2string (outname));
	  scm_gc_unprotect_object (paper_book->self_scm ());
	}
      delete output;
    }
}
 
SCM
Score::book_rendering (String outname,
		       Book_paper_def* paperbook,
		       Music_output_def *default_def,
		       Paper_def **paper)
{
  SCM out = scm_makfrom0str (outname.to_str0 ());
  SCM systems = SCM_EOL;
  int outdef_count = defs_.size ();
  for (int i = 0; !i || i < outdef_count; i++)
    {
      Music_output_def *def = outdef_count ? defs_[i] : default_def;
      if (Paper_def * pd = dynamic_cast<Paper_def*> (def))
	{
	  def = paperbook->scale_paper (pd);
	}
      
      if (!(no_paper_global_b && dynamic_cast<Paper_def*> (def)))
	{
	  SCM context = ly_run_translator (music_, def->self_scm ());
	  if (Global_context *g = dynamic_cast<Global_context*>
	      (unsmob_context (context)))
	    {
	      SCM s = ly_format_output (context, out);
	      if (s != SCM_UNDEFINED)
		{
		  systems = s;
		  /* Ugh. */
		  Music_output *output = g->get_output ();
		  if (Paper_score *ps = dynamic_cast<Paper_score*> (output))
		    *paper = ps->paper_;
		}
	    }
	}
    }
  return systems;
}

LY_DEFINE (ly_score_bookify, "ly:score-bookify",
	   1, 0, 0,
	   (SCM score_smob),
	   "Return SCORE encapsulated in a BOOK.")
{
  SCM_ASSERT_TYPE (unsmob_score (score_smob), score_smob, SCM_ARG1, __FUNCTION__, "score_smob");
  
  Score *score = unsmob_score (score_smob);
  Book *book = new Book;
  book->scores_.push (score);
  scm_gc_unprotect_object (book->self_scm ());
  return book->self_scm ();
}
