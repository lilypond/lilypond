/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "line-of-score.hh"
#include "spanner.hh"
#include "atom.hh"
#include "paper-def.hh"
#include "p-col.hh"
#include "p-score.hh"


Line_of_score::Line_of_score()
{
  error_mark_b_ = 0;
}


IMPLEMENT_IS_TYPE_B1(Line_of_score,Spanner);

void
Line_of_score::add_element (Score_element*e)
{
  // avoid excess dependencies.
  if (!(e->axis_group_l_a_[0] || e->axis_group_l_a_[1]))
    add_dependency (e);
}

bool
Line_of_score::contains_b (Paper_column const* c) const
{
  return cols.find_l ((Paper_column*)c);
}

Line_of_score*
Line_of_score::set_breaking (Array<Column_x_positions> const &breaking, int j) const
{
  const Array<Paper_column*> &curline (breaking[j].cols);
  const Array<Paper_column*> &errors (breaking[j].error_col_l_arr_);
  const Array<Real> &config (breaking[j].config);
	
  for (int i=0; i < errors.size(); i++)
    errors[i]->error_mark_b_ = true;

  Line_of_score *line_l=0;
	
  if (breaking.size() >1) 
    {
      line_l = (Line_of_score*)clone()->access_Spanner ();
    }
  else 
    line_l = (Line_of_score*) this;
	
  ((Array<Paper_column*> &)line_l->cols) = curline;
  line_l->set_bounds(LEFT,curline[0]);
      
  line_l->set_bounds(RIGHT,curline.top());
	
  for (int i=0; i < curline.size(); i++)
    {
      curline[i]->translate_axis (config[i],X_AXIS);
      curline[i]->line_l_ = (Line_of_score*)line_l;
    }

  return line_l;
}




void
Line_of_score::do_print() const
{
  Spanner::do_print();
}

Interval
Line_of_score::do_width() const
{ 
  return Spanner::do_width();
}

Link_array<Score_element>
Line_of_score::get_extra_dependencies () const
{
  Link_array<Score_element> r;
  for (int i=0; i < cols.size (); i++)
    r.push (cols[i]);
  return r;
}

void
Line_of_score::do_unlink () 
{
  Spanner::do_unlink ();
  for (int i=0; i < cols.size (); i++)
    cols[i]->line_l_ =0;
  cols.set_size (0);
}


void
Line_of_score::do_junk_links () 
{
  cols.set_size (0);
}
