/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "line-of-score.hh"
#include "spanner.hh"

#include "paper-def.hh"
#include "p-col.hh"
#include "p-score.hh"


Line_of_score::Line_of_score()
{
  error_mark_b_ = 0;
}




void
Line_of_score::add_element (Score_element*e)
{
  // avoid excess dependencies.
  if (! (e->parent_l (X_AXIS) || e->parent_l (Y_AXIS)) )
    add_dependency (e);
}

bool
Line_of_score::contains_b (Paper_column const* c) const
{
  return cols_.find_l ((Paper_column*)(c));
}

Line_of_score*
Line_of_score::set_breaking (Array<Column_x_positions> const &breaking, int j) const
{
  const Link_array<Paper_column> &curline (breaking[j].cols);
  const Link_array<Paper_column> &errors (breaking[j].error_col_l_arr_);
  const Array<Real> &config (breaking[j].config);
	
  for (int i=0; i < errors.size(); i++)
    errors[i]->error_mark_b_ = true;

  Line_of_score *line_l=0;
	
  if (breaking.size() >1) 
    {
      line_l = dynamic_cast <Line_of_score*> (clone());
    }
  else 
    line_l = (Line_of_score*)( this);

  line_l->cols_ = curline;
  /*  Array<Paper_column*> &
      ((Array<Paper_column*> &)) = */
  line_l->set_bounds(LEFT,curline[0]);
      
  line_l->set_bounds(RIGHT,curline.top());
	
  for (int i=0; i < curline.size(); i++)
    {
      curline[i]->translate_axis (config[i],X_AXIS);
      curline[i]->line_l_ = dynamic_cast<Line_of_score*>(line_l);
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
  for (int i=0; i < cols_.size (); i++)
    r.push (cols_[i]);
  return r;
}

void
Line_of_score::do_unlink () 
{
  Spanner::do_unlink ();
  for (int i=0; i < cols_.size (); i++)
    cols_[i]->line_l_ =0;
  cols_.set_size (0);
}


void
Line_of_score::do_junk_links () 
{
  cols_.set_size (0);
}
