/*
  score.cc -- implement Score

  source file of the GNU LilyPond music typesetter

  (c) 1997--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "score.hh"

#include <cstdio>

using namespace std;

#include "book.hh"
#include "cpu-timer.hh"
#include "global-context.hh"
#include "international.hh"
#include "lily-parser.hh"
#include "lilypond-key.hh"
#include "main.hh"
#include "music.hh"
#include "music.hh"
#include "output-def.hh"
#include "paper-book.hh"
#include "paper-score.hh"
#include "warn.hh"

#include "ly-smobs.icc"

Input *
Score::origin () const
{
  return unsmob_input (input_location_);
}


Score::Score ()
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;
  input_location_ = SCM_EOL;

  error_found_ = false;

  smobify_self ();
  input_location_ = make_input (Input ());
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
  Score *sc = (Score *) SCM_CELL_WORD_1 (s);

  scm_gc_mark (sc->header_);
  for (vsize i = sc->defs_.size (); i--;)
    scm_gc_mark (sc->defs_[i]->self_scm ());

  scm_gc_mark (sc->input_location_);
  return sc->music_;
}

int
Score::print_smob (SCM, SCM p, scm_print_state*)
{
  scm_puts ("#<Score>", p);

  return 1;
}

Score::Score (Score const &s)
{
  header_ = SCM_EOL;
  music_ = SCM_EOL;
  input_location_ = SCM_EOL;
  error_found_ = s.error_found_;

  smobify_self ();
  input_location_ = make_input (*s.origin ()); 

  Music *m = unsmob_music (s.music_);
  if (m)
    {
      Music *mclone = m->clone ();
      music_ = mclone->unprotect ();
    }
  else
    music_ = SCM_EOL;

  for (vsize i = 0, n = s.defs_.size (); i < n; i++)
    {
      Output_def *copy = s.defs_[i]->clone ();
      defs_.push_back (copy);
      copy->unprotect ();
    }
  header_ = ly_make_anonymous_module (false);
  if (ly_is_module (s.header_))
    ly_module_copy (header_, s.header_);
}

void
default_rendering (SCM music, SCM outdef,
		   SCM book_outputdef,
		   SCM header,
		   SCM outname,
		   SCM key)
{
  SCM scaled_def = outdef;
  SCM scaled_bookdef = book_outputdef;

  Output_def *bpd = unsmob_output_def (book_outputdef);

  /* ugh.  */
  if (bpd->c_variable ("is-paper") == SCM_BOOL_T)
    {
      Real scale = scm_to_double (bpd->c_variable ("output-scale"));

      Output_def *def = scale_output_def (unsmob_output_def (outdef), scale);
      Output_def *bdef = scale_output_def (bpd, scale);
      def->parent_ = bdef;

      scaled_def = def->self_scm ();
      scaled_bookdef = bdef->self_scm ();

      def->unprotect ();
      bdef->unprotect ();
    }

  SCM context = ly_run_translator (music, scaled_def, key);
  
  SCM output_as_scm = ly_format_output (context);
  Music_output *output = unsmob_music_output (output_as_scm);

  if (Paper_score *pscore = dynamic_cast<Paper_score *> (output))
    {
      /* ugh, this is strange, Paper_book without a Book object. */
      Paper_book *paper_book = new Paper_book ();
      paper_book->header_ = header;
      paper_book->paper_ = unsmob_output_def (scaled_bookdef);

      if (ly_is_module (header))
	paper_book->add_score (header);

      paper_book->add_score (pscore->self_scm ());
      paper_book->classic_output (outname);
      paper_book->unprotect ();
    }

  scm_remember_upto_here_1 (scaled_def);
  scm_remember_upto_here_1 (output_as_scm);
  scm_remember_upto_here_1 (scaled_bookdef);
}

/*
  Format score, return list of Music_output objects.

  LAYOUTBOOK should be scaled already.
*/
SCM
Score::book_rendering (Output_def *layoutbook,
		       Output_def *default_def,
		       Object_key *book_key)
{
  if (error_found_)
    return SCM_EOL;

  SCM scaled_bookdef = SCM_EOL;
  Real scale = 1.0;

  if (layoutbook && layoutbook->c_variable ("is-paper") == SCM_BOOL_T)
    scale = scm_to_double (layoutbook->c_variable ("output-scale"));

  SCM outputs = SCM_EOL;
  SCM *tail = &outputs;

  int outdef_count = defs_.size ();

  Object_key *key = new Lilypond_general_key (book_key, user_key_, 0);
  SCM scm_key = key->unprotect ();

  for (int i = 0; !i || i < outdef_count; i++)
    {
      Output_def *def = outdef_count ? defs_[i] : default_def;
      SCM scaled = SCM_EOL;

      if (def->c_variable ("is-layout") == SCM_BOOL_T)
	{
	  def = scale_output_def (def, scale);
	  def->parent_ = layoutbook;

	  scaled = def->unprotect ();
	}

      /* TODO: fix or junk --no-layout.  */
      SCM context = ly_run_translator (music_, def->self_scm (), scm_key);
      if (dynamic_cast<Global_context *> (unsmob_context (context)))
	{
	  SCM s = ly_format_output (context);

	  *tail = scm_cons (s, SCM_EOL);
	  tail = SCM_CDRLOC (*tail);
	}

      scm_remember_upto_here_1 (scaled);
    }

  scm_remember_upto_here_1 (scm_key);
  scm_remember_upto_here_1 (scaled_bookdef);
  return outputs;
}

void
Score::set_music (SCM music)
{
  if (unsmob_music (music_))
    {
      unsmob_music (music)->origin ()->error (_ ("already have music in score"));
      unsmob_music (music_)->origin ()->error (_ ("this is the previous music"));
    }
  Music *m = unsmob_music (music);
  if (m && to_boolean (m->get_property ("error-found")))
    {
      m->origin ()->error (_ ("errors found, ignoring music expression"));

      this->error_found_ = this->error_found_
	|| to_boolean (m->get_property ("error-found"));
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

void
Score::add_output_def (Output_def *def)
{
  defs_.push_back (def);
}
