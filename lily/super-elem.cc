/*
  super-elem.cc -- implement Super_elem

  source file of the LilyPond music typesetter

  (c)  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "super-elem.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "string.hh"
#include "outputter.hh"

void
Super_elem::handle_broken_dependencies()
{
  lines_arr_ = line_of_score_l_->get_lines();
  for (int i =0; i < lines_arr_.size(); i++) 
    add_dependency (lines_arr_[i]);
}
  

void
Super_elem::do_substitute_dependency (Score_elem*o,Score_elem* n)
{
  if (line_of_score_l_ == o->spanner())
    line_of_score_l_ = n? (Line_of_score*) n->spanner() : 0;
}

Super_elem::Super_elem()
{
  line_of_score_l_ = new Line_of_score ;
}

void
Super_elem::do_add_processing()
{
  pscore_l_->typeset_unbroken_spanner (line_of_score_l_);
  add_dependency (line_of_score_l_);
}

IMPLEMENT_IS_TYPE_B1(Super_elem,Score_elem);


/**
    for administration of what was done already
    */
enum Score_elem_status {
  ORPHAN=0,			// not yet added to pstaff
  VIRGIN,			// added to pstaff
  PREBROKEN,
  PRECALCING,
  PRECALCED,		// calcs before spacing done
  SPACING,
  SPACED,
  BROKEN,
  POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
  POSTCALCED,		// after spacing calcs done
  BREWING,
  BREWED,
  UNLINKING,
  UNLINKED,
};

void
Super_elem::pre_processing ()
{
  calcalute_dependencies (PRECALCING, PRECALCED, &Score_elem::do_pre_processing);
}

void
Super_elem::space_processing ()
{
  calcalute_dependencies (SPACING, SPACED, &Score_elem::do_space_processing);
}

/* for break processing, use only one status, because copies have to
  have correct status. (Previously,
  Score_elem::handle_[pre]broken_dependencies assigned to status_i_
  */
void
Super_elem::breakable_col_processing ()
{
  calcalute_dependencies (PREBROKEN, PREBROKEN, &Score_elem::do_breakable_col_processing);
}

void
Super_elem::break_processing ()
{
  calcalute_dependencies (BROKEN, BROKEN, &Score_elem::do_break_processing);
}
void
Super_elem::post_processing ()
{
  calcalute_dependencies (POSTCALCING, POSTCALCED, &Score_elem::do_post_processing);
}

void
Super_elem::output_all () 
{
  for (int i=0; i < lines_arr_.size(); i++)
    {
      pscore_l_->outputter_l_->start_line ();
      lines_arr_[i]->calcalute_dependencies (BREWING, BREWED, &Score_elem::do_brew_molecule);
      pscore_l_->outputter_l_->stop_line ();
    }
}



void
Super_elem::unlink_all ()
{
  calcalute_dependencies (UNLINKING, UNLINKED, &Score_elem::junk_links);
}

