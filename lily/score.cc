/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2005 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score.hh"

#include <cstdio>

#include "lilypond-key.hh"
#include "lily-parser.hh"
#include "book.hh"
#include "cpu-timer.hh"
#include "global-context.hh"
#include "ly-smobs.icc"
#include "main.hh"
#include "music-iterator.hh"
#include "output-def.hh"
#include "music.hh"
#include "paper-book.hh"
#include "output-def.hh"
#include "paper-score.hh"
#include "scm-hash.hh"
#include "warn.hh"

Score::Score ()
  : Input ()
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;
  error_found_ = false;
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

#if 0 
  if (sc->key_)
    scm_gc_mark (sc->key_->self_scm());
#endif
  
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
  error_found_ = s.error_found_;
  
  /* FIXME: SCM_EOL? */
  header_ = 0;

  smobify_self ();

  Music *m = unsmob_music (s.music_);
  music_ = m ? m->clone ()->self_scm () : SCM_EOL;
  scm_gc_unprotect_object (music_);
  
  for (int i = 0; i < s.defs_.size (); i++)
    defs_.push (s.defs_[i]->clone ());

  header_ = ly_make_anonymous_module (false);
  if (ly_c_module_p (s.header_))
    ly_module_copy (header_, s.header_);
}


void
default_rendering (SCM music, SCM outdef,
		   SCM book_outputdef,
		   SCM header, SCM outname,
		   SCM key)
{
  SCM scaled_def = outdef;
  SCM scaled_bookdef = book_outputdef;
  
  Output_def *bpd = unsmob_output_def (book_outputdef);

  /* ugh.  */
  if (bpd->c_variable ("is-paper") == SCM_BOOL_T)
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
  
  SCM context = ly_run_translator (music, scaled_def, key);
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
	  paper_book->paper_ = unsmob_output_def (scaled_bookdef);
	  
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

LAYOUTBOOK should be scaled already.

*/
SCM
Score::book_rendering (String outname,
		       Output_def *layoutbook,
		       Output_def *default_def,
		       Object_key *book_key)
{
  if (error_found_)
    return SCM_EOL;
   
  SCM scaled_bookdef = SCM_EOL;
  Real scale = 1.0;

  if (layoutbook && layoutbook->c_variable ("is-paper") == SCM_BOOL_T)
    scale = scm_to_double (layoutbook->c_variable ("outputscale"));
  
  SCM out = scm_makfrom0str (outname.to_str0 ());
  SCM systems = SCM_EOL;
  int outdef_count = defs_.size ();

  Object_key * key = new Lilypond_general_key (book_key, user_key_, 0);
  SCM scm_key = key->self_scm();
  scm_gc_unprotect_object (scm_key);
  
  for (int i = 0; !i || i < outdef_count; i++)
    {
      Output_def *def = outdef_count ? defs_[i] : default_def;
      SCM scaled = SCM_EOL;
      if (def->c_variable ("is-layout") == SCM_BOOL_T)
	{
	  def = scale_output_def (def, scale);
	  def->parent_ = layoutbook;
	  scaled = def->self_scm ();
	  scm_gc_unprotect_object (scaled);
	}

      /* TODO: fix or junk --no-layout.  */
      SCM context = ly_run_translator (music_, def->self_scm (), scm_key);
      if (dynamic_cast<Global_context*> (unsmob_context (context)))
	{
	  SCM s = ly_format_output (context, out);
	  if (s != SCM_UNDEFINED)
	    systems = s;
	}

      scm_remember_upto_here_1 (scaled);
    }

  scm_remember_upto_here_1 (scm_key);
  scm_remember_upto_here_1 (scaled_bookdef);
  return systems;
}





void
Score::set_music (SCM music, SCM parser)
{
  /* URG? */
  SCM check_funcs = ly_lily_module_constant ("toplevel-music-functions");
  for (; scm_is_pair (check_funcs); check_funcs = scm_cdr (check_funcs))
    music = scm_call_2 (scm_car (check_funcs), music, parser);

  if (unsmob_music (music_))
    {
      unsmob_music (music)->origin ()->error (_("Already have music in score"));
      unsmob_music (music_)->origin ()->error (_("This is the previous music"));
    }
  Music * m = unsmob_music (music);
  if (m && to_boolean (m->get_property ("error-found")))
    {
      m->origin()->error (_("Error found in this music expression. Ignoring it"));
      
      this->error_found_ = this->error_found_ || to_boolean (m->get_property ("error-found"));
      
    }

  if (this->error_found_)
    this->music_ = SCM_EOL; 
  else
    this->music_ = music;

}

SCM
Score::get_music () const
{
  return music_;
}
