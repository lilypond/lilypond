/*
  p-score.cc -- implement Paper_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "main.hh"
#include "debug.hh"
#include "lookup.hh"
#include "spanner.hh"
#include "paper-def.hh"
#include "line-of-score.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "paper-column.hh"
#include "scope.hh"
#include "gourlay-breaking.hh"
#include "paper-stream.hh"
#include "paper-outputter.hh"
#include "file-results.hh"
#include "misc.hh"
#include "all-font-metrics.hh"

Paper_score::Paper_score ()
{
  paper_l_ =0;
  outputter_l_ =0;
  Line_of_score * line_p = new Line_of_score;
  line_p->pscore_l_ = this;
  element_smob_list_ = scm_protect_object (gh_cons (line_p->self_scm_, SCM_EOL));
  line_l_ = line_p;
}

Paper_score::Paper_score (Paper_score const &s)
  : Music_output (s)
{
  assert (false);
}

Paper_score::~Paper_score ()
{
  scm_unprotect_object (element_smob_list_);
}

void
Paper_score::typeset_element (Score_element * elem_p)
{
  elem_p->pscore_l_ = this;

  gh_set_cdr_x(element_smob_list_,
	       gh_cons (elem_p->self_scm_, gh_cdr (element_smob_list_)));
  elem_p->set_elt_property ("full-name",
			    gh_str02scm((char*)elem_p->name()));
  
  scm_unprotect_object (elem_p->self_scm_);
}

void
Paper_score::add_column (Paper_column *p)
{
  p->set_rank (col_l_arr_.size ());
  col_l_arr_.push (p);
  typeset_element(p);
}

void
Paper_score::print () const
{
#ifndef NPRINT
  if (!flower_dstream)
    return ;

  DEBUG_OUT << "Paper_score { ";
  DEBUG_OUT << "\n elements: ";

  for (SCM p = gh_cdr (element_smob_list_);
       p != SCM_EOL;
       p = gh_cdr(p))
    gh_display (gh_car (p));
  DEBUG_OUT << "}\n";
#endif
}

int
Paper_score::find_col_idx (Paper_column const *c) const
{
  Paper_column const *what = c;

  return col_l_arr_.find_i ((Paper_column*)what);
}

Array<Column_x_positions>
Paper_score::calc_breaking ()
{
  Break_algorithm *algorithm_p=0;
  Array<Column_x_positions> sol;

  algorithm_p = new Gourlay_breaking ;
  algorithm_p->set_pscore (this);
  sol = algorithm_p->solve ();
  delete algorithm_p;

  return sol;
}



void
Paper_score::process ()
{
  print ();
  progress_indication (_ ("Preprocessing elements...") + " ");


  /*
    Be sure to set breakability on first & last column.
   */
  col_l_arr_[0]->set_elt_property ("breakable", SCM_BOOL_T);
  col_l_arr_.top ()->set_elt_property ("breakable", SCM_BOOL_T);

  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->do_breakable_col_processing ();

  fixup_refpoints ();

  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      Score_element* sc = unsmob_element (gh_car (s));
      sc->calculate_dependencies (PRECALCED, PRECALCING, &Score_element::before_line_breaking);
    }

  progress_indication ("\n" + _ ("Calculating column positions...") + " " );
  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    unsmob_element (gh_car (s))->do_space_processing ();

  Array<Column_x_positions> breaking = calc_breaking ();
  line_l_->break_into_pieces (breaking);

  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      unsmob_element (gh_car (s))->do_break_processing ();
    }
  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      unsmob_element (gh_car (s))->handle_broken_dependencies ();
    }
  
  outputter_l_ = new Paper_outputter ;
  outputter_l_->output_header ();
  outputter_l_->output_version();
  
  if (header_global_p)
    outputter_l_->output_scope (header_global_p, "mudela");
  if (header_l_)
    outputter_l_->output_scope (header_l_, "mudela");

  outputter_l_->output_comment (_ ("Outputting Score, defined at: "));
  outputter_l_->output_comment (origin_str_);

  if (paper_l_->scope_p_)
    outputter_l_->output_scope (paper_l_->scope_p_, "mudelapaper");
  
  SCM scm = gh_list (ly_symbol2scm ("experimental-on"), SCM_UNDEFINED);
  outputter_l_->output_scheme (scm);
  scm = gh_list (ly_symbol2scm ("header-end"), SCM_UNDEFINED);
  outputter_l_->output_scheme (scm);


  /*
    This is tricky: we have to put the font definitions before the
    actual output, but we don't know all fonts in advanced: generating
    the output might trigger loading of a new font.  So we store the
    place to insert the font definitions, generate the output and then
    insert the definitions
    
   */
  SCM before_output = outputter_l_->last_cons_;
  fixup_refpoints ();

  /*
    TODO: change this, so that each element ouputs its molecules into
    its line, and then output all lines one by one; then we can do
    
    foreach element: output
  */
  line_l_->output_lines ();


  SCM font_names = ly_quote_scm (all_fonts_global_p->font_descriptions ());
  gh_set_cdr_x (before_output,
		gh_cons  (gh_list (ly_symbol2scm ("define-fonts"),
				   font_names,
				   SCM_UNDEFINED),
			  gh_cdr (before_output)));
  
  Paper_stream* psp = paper_l_->paper_stream_p ();
  outputter_l_->dump_onto (psp);

  // huh?
  delete outputter_l_;
  outputter_l_ = 0;
  delete psp;
  
}

Link_array<Item>
Paper_score::broken_col_range (Item const*l, Item const*r) const
{
  Link_array<Item> ret;

  l = l->column_l ();
  r = r->column_l ();
  
  int  start = l
    ? find_col_idx (dynamic_cast<Paper_column*> ((Item*)l))+1
    : 0;

  int stop = r
    ? find_col_idx (dynamic_cast<Paper_column*>((Item*)r))
    : col_l_arr_.size ();

  while (start < stop)
    {
      Paper_column *c = col_l_arr_[start];
      if (c->breakable_b () && !c->line_l_)
	ret.push (c);
      start++;
    }

  return ret;
}


void
Paper_score::fixup_refpoints ()
{
  for (SCM s = element_smob_list_; gh_pair_p (s); s = gh_cdr (s))
    {
      SCM e = gh_car (s);
      if (SMOB_IS_TYPE_B(Score_element, e))
	{
	  Score_element * se = unsmob_element (e);
	  se->fixup_refpoint ();

	  if (!dynamic_cast<Line_of_score*> (se) && !se->parent_l (Y_AXIS))
	    {
	      programming_error ("No parent!");
	    }
	}
    }
}
