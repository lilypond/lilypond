/*
  super-elem.cc -- implement Super_elem

  source file of the LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "super-elem.hh"
#include "scoreline.hh"
#include "p-score.hh"
#include "string.hh"

String
Super_elem::TeX_output_str() const
{
  String s;
  for (int i=0; i < lines_arr_.size(); i++)
    {
      s += lines_arr_[i]->TeX_output_str();
      if (i + 1<lines_arr_.size())
	s += "\\interscoreline"; // TODO
    }
  return s;
}

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
