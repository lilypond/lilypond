/*
  scoreline.cc -- implement Line_of_score

  source file of the GNU LilyPond music typesetter

  (c) 1996,  1997--1998 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "scoreline.hh"
#include "dimen.hh"
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
Line_of_score::add (Score_elem*e)
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

void
Line_of_score::set_breaking (Array<Col_hpositions> const &breaking)
{
  for (int j=0; j < breaking.size(); j++) 
    {
      const Array<Paper_column*> &curline (breaking[j].cols);
      const Array<Paper_column*> &errors (breaking[j].error_col_l_arr_);
      const Array<Real> &config (breaking[j].config);
	
      for (int i=0; i < errors.size(); i++)
	errors[i]->error_mark_b_ = true;

      Line_of_score *line_l=0;
      Line_of_score *line_p =0;
	
      if (breaking.size() >1) 
	{
	  line_p = (Line_of_score*)clone()->spanner ();
	  line_l = line_p;
	}
      else 
	line_l =  this;
	
      ((Array<Paper_column*> &)line_l->cols) = curline;
      line_l->set_bounds(LEFT,curline[0]);
      
      line_l->set_bounds(RIGHT,curline.top());
	
      if (line_p) 
	{
	  pscore_l_->typeset_broken_spanner (line_p);
	  broken_into_l_arr_.push (line_p);
	}

      for (int i=0; i < curline.size(); i++)
	{
	  curline[i]->translate_axis (config[i],X_AXIS);
	  curline[i]->line_l_ = (Line_of_score*)line_l;
	}
    }
}


void
Line_of_score::break_into_pieces (bool)
{
  
}

Link_array<Line_of_score>
Line_of_score::get_lines() const
{
  Link_array<Line_of_score> ret;

  if (broken_into_l_arr_.size())
    for (int i=0; i < broken_into_l_arr_.size(); i++) 
      {
	ret.push ((Line_of_score*)broken_into_l_arr_[i]);
      }
  else 
    ret.push ((Line_of_score*)this);	// ugh
  
  return ret;
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

Link_array<Score_elem>
Line_of_score::get_extra_dependencies () const
{
  Link_array<Score_elem> r;
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
