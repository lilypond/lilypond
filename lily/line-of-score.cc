/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "line-of-score.hh"
#include "paper-def.hh"
#include "paper-outputter.hh"
#include "paper-column.hh"
#include "paper-score.hh"
#include "warn.hh"
#include "main.hh"
#include "debug.hh"

Line_of_score::Line_of_score()
{
  set_elt_property ("columns", SCM_EOL);
  set_axes (Y_AXIS,X_AXIS);
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
  add_element (p);
}

void
Line_of_score::do_print() const
{
  Spanner::do_print();
  Axis_group_spanner::do_print ();
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
  Super_element::output_all ();
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
