/*
  paper-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "score.hh"
#include "main.hh"
#include "warn.hh"
#include "font-metric.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "system.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "scm-hash.hh"
#include "gourlay-breaking.hh"
#include "paper-outputter.hh"
#include "input-file-results.hh"
#include "misc.hh"
#include "all-font-metrics.hh"

#include "stencil.hh"
#include "paper-book.hh"
#include "ly-module.hh"

Paper_score::Paper_score ()
{
  paper_ = 0;
  outputter_ = 0;
  system_ = 0;
  main_smob_ = SCM_EOL;
  lines_ = SCM_EOL;
  book_title_ = 0;
  score_title_ = 0;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}


void
Paper_score::typeset_line (System *line)
{
  if (!system_)
    /* ugh. */
    system_ = line;

  main_smob_ = gh_cons (line->self_scm (), main_smob_);
  line->pscore_ = this;

  scm_gc_unprotect_object (line->self_scm ());
}

Array<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm=0;
  Array<Column_x_positions> sol;

  algorithm = new Gourlay_breaking ;
  algorithm->set_pscore (this);
  sol = algorithm->solve ();
  delete algorithm;

  return sol;
}

void
Paper_score::process (String outname)
{
  if (verbose_global_b)
    progress_indication (_f ("Element count %d (spanners %d) ",
			     system_->element_count (),
			     system_->spanner_count ()));

  progress_indication (_ ("Preprocessing graphical objects...") + " ");

  /* FIXME: Check out why we need this - removing gives assertion failures
     down the road.
     
     doubly, also done in Score_engraver */
  Link_array<Grob> pc (system_->columns ());
  pc[0]->set_property ("breakable", SCM_BOOL_T);
  pc.top ()->set_property ("breakable", SCM_BOOL_T);
    
  system_->pre_processing ();
 
  Array<Column_x_positions> breaking = calc_breaking ();
  system_->break_into_pieces (breaking);
  lines_ = system_->get_lines ();

  // FIXME: ...
  outputter_ = paper_->get_paper_outputter (outname);

  progress_indication ("\n");

  SCM scopes = SCM_EOL;
  if (header_)
    scopes = scm_cons (header_, scopes);
  if (global_input_file->header_ && global_input_file->header_ != header_)
    scopes = scm_cons (global_input_file->header_, scopes);
  
  outputter_->output_metadata (scopes, paper_);

#ifdef PAGE_LAYOUT
  SCM make_title = scm_primitive_eval (ly_symbol2scm ("make-title"));
  SCM b = ly_modules_lookup (scopes, ly_symbol2scm ("bookTitle"));
  if (b != SCM_UNDEFINED && scm_variable_bound_p (b) == SCM_BOOL_T)
    book_title_
      = unsmob_stencil (gh_call2 (make_title, paper_->self_scm (),
				  scm_variable_ref (b)));
  
  SCM s = ly_modules_lookup (scopes, ly_symbol2scm ("scoreTitle"));
  if (s != SCM_UNDEFINED && scm_variable_bound_p (s) == SCM_BOOL_T)
    score_title_
      = unsmob_stencil (gh_call2 (make_title, paper_->self_scm (),
				  scm_variable_ref (s)));
  
#if 0
  // FIXME: 
  delete system_;
  system_ = 0;
#endif
  
  /* Ugh: caller (Score) should do this, but does not know our flavor
     (paper or midi).  */
  paper_book->paper_scores_.push (this);
#else
  output ();
#endif  
}

void
Paper_score::output ()
{
  outputter_->output_header (paper_);
  
  int line_count = SCM_VECTOR_LENGTH ((SCM) lines_);
  for (int i = 0; i < line_count; i++)
    outputter_->output_line (scm_vector_ref (lines_, scm_int2num (i)),
			     i == line_count - 1);
  
  outputter_->output_scheme (scm_list_1 (ly_symbol2scm ("end-output")));
  progress_indication ("\n");

#if 0  
  // huh?
  delete outputter_;
  outputter_ = 0;
#endif  
}
