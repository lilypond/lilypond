/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "axis-group-interface.hh"
#include "debug.hh"
#include "line-of-score.hh"
#include "main.hh"
#include "paper-column.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-score.hh"
#include "string.hh"
#include "warn.hh"


Line_of_score::Line_of_score()
{
  set_elt_property ("columns", SCM_EOL);
  Axis_group_interface (this).set_interface ();
  Axis_group_interface (this).set_axes (Y_AXIS,X_AXIS);
}



void
Line_of_score::output_lines ()
{
  for (int i=0; i < broken_into_l_arr_.size (); i++)
    {
      Line_of_score *line_l = dynamic_cast<Line_of_score*> (broken_into_l_arr_[i]);

      progress_indication ("[");
      line_l->post_processing ();
      progress_indication (to_str (i));
      line_l->output_line (i + 1 == broken_into_l_arr_.size ());
      progress_indication ("]");
    }
}

// const?
void
Line_of_score::break_into_pieces (Array<Column_x_positions> const &breaking)
{
  for (int i=0; i < breaking.size (); i++)
    {
      Line_of_score *line_l = dynamic_cast <Line_of_score*> (clone());
      line_l->rank_i_ = i;
      Link_array<Paper_column> c (breaking[i].cols_);
      pscore_l_->typeset_element (line_l);
      line_l->set_bound(LEFT,c[0]);
      line_l->set_bound(RIGHT,c.top ());
      for (int j=0; j < c.size(); j++)
	{
	  c[j]->translate_axis (breaking[i].config_[j],X_AXIS);
	  c[j]->line_l_ = line_l;
	}
      
      broken_into_l_arr_.push (line_l);
    }
}

void
Line_of_score::add_column (Paper_column*p)
{
  set_elt_property ("columns",
		    gh_cons (p->self_scm_, get_elt_property ("columns")));
  Axis_group_interface (this).add_element (p);
}


void
Line_of_score::output_line (bool last_line)
{
  Interval i(extent(Y_AXIS));
  if (i.empty_b())
    programming_error ("Huh?  Empty Line_of_score?");
  else
    translate_axis (- i[MAX], Y_AXIS);
  
  pscore_l_->outputter_l_->start_line (i.length ());
  output_all ();
  if (last_line)
    pscore_l_->outputter_l_->stop_last_line();
  else
    pscore_l_->outputter_l_->stop_line ();
}

int
Line_of_score::compare (Line_of_score* const &p1,Line_of_score* const &p2)
{
  return p1->rank_i_ - p2->rank_i_;
}



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
  BROKEN_SECOND,
  POSTCALCING,		// busy calculating. This is used to trap cyclic deps.
  POSTCALCED,		// after spacing calcs done
  BREWING,
  BREWED,
};

void
Line_of_score::pre_processing ()
{
  calculate_dependencies (PRECALCED, PRECALCING, &Score_element::before_line_breaking);
}

void
Line_of_score::space_processing ()
{
  calculate_dependencies (SPACED, SPACING, &Score_element::do_space_processing);
}

/* for break processing, use only one status, because copies have to
  have correct status. (Previously,
  Score_element::handle_[pre]broken_dependencies assigned to status_i_
  */
void
Line_of_score::breakable_col_processing ()
{
  calculate_dependencies (PREBROKEN, PREBROKEN, &Score_element::do_breakable_col_processing);
  //  calculate_dependencies (PREBROKEN_SECOND, PREBROKEN_SECOND, &Score_element::handle_prebroken_dependents);
}


void
Line_of_score::post_processing ()
{
  //  calculate_dependencies (BROKEN_SECOND, BROKEN_SECOND,
  //		  &Score_element::handle_broken_dependents);
  calculate_dependencies (POSTCALCED, POSTCALCING, &Score_element::after_line_breaking);
}

void
Line_of_score::output_all () 
{
  calculate_dependencies (BREWED, BREWING, &Score_element::output_processing);
}




