/*
  super-elem.cc -- implement Super_elem

  source file of the LilyPond music typesetter

  (c)  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "super-element.hh"
#include "line-of-score.hh"
#include "p-score.hh"
#include "string.hh"
#include "paper-outputter.hh"


/**
    for administration of what was done already
    */
enum Score_element_status {
  ORPHAN=0,			// not yet added to pstaff
  VIRGIN,			// added to pstaff
  PREBROKEN,
  PREBROKEN_SECOND,
  PRECALCING,
  PRECALCED,		// calcs before spacing done
  SPACING,
  SPACED,
  BROKEN,
  POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
  POSTCALCED,		// after spacing calcs done
  BREWING,
  BREWED,
};

void
Super_element::pre_processing ()
{
  calculate_dependencies (PRECALCED, PRECALCING, &Score_element::do_pre_processing);
}

void
Super_element::space_processing ()
{
  calculate_dependencies (SPACED, SPACING, &Score_element::do_space_processing);
}

/* for break processing, use only one status, because copies have to
  have correct status. (Previously,
  Score_element::handle_[pre]broken_dependencies assigned to status_i_
  */
void
Super_element::breakable_col_processing ()
{
  calculate_dependencies (PREBROKEN, PREBROKEN, &Score_element::do_breakable_col_processing);
  calculate_dependencies (PREBROKEN_SECOND, PREBROKEN_SECOND, &Score_element::handle_prebroken_dependents);
}

void
Super_element::break_processing ()
{
  calculate_dependencies (BROKEN, BROKEN, &Score_element::do_break_processing);
}
void
Super_element::post_processing ()
{
  calculate_dependencies (POSTCALCED, POSTCALCING, &Score_element::do_post_processing);
}

void
Super_element::output_all () 
{
  calculate_dependencies (BREWED, BREWING, &Score_element::output_processing);
}




